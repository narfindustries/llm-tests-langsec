#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Parser declarations
static HParser *transaction_id;
static HParser *protocol_id;
static HParser *length;
static HParser *unit_id;
static HParser *function_code;
static HParser *starting_address;
static HParser *quantity;
static HParser *byte_count;
static HParser *output_address;
static HParser *output_value;
static HParser *exception_code;

static HParsedToken* get_uint_action(const HParseResult *p, void* user_data) {
    return (HParsedToken*)p->ast;
}

static HParser* parse_read_request() {
    return h_sequence(starting_address, quantity, NULL);
}

static HParser* parse_write_single() {
    return h_sequence(output_address, output_value, NULL);
}

static HParser* parse_write_multiple_request() {
    return h_sequence(starting_address, 
                     quantity,
                     byte_count,
                     h_length_value(byte_count, h_uint8()),
                     NULL);
}

static HParser* parse_read_response() {
    return h_sequence(byte_count,
                     h_length_value(byte_count, h_uint8()),
                     NULL);
}

static HParser* parse_write_single_response() {
    return h_sequence(output_address, output_value, NULL);
}

static HParser* parse_write_multiple_response() {
    return h_sequence(starting_address, quantity, NULL);
}

static HParser* parse_exception_response() {
    return h_sequence(h_uint8(), exception_code, NULL);
}

static HParser* init_modbus_parser() {
    // Initialize all parser components
    transaction_id = h_uint16();
    protocol_id = h_uint16();
    length = h_uint16();
    unit_id = h_uint8();
    function_code = h_uint8();
    starting_address = h_uint16();
    quantity = h_uint16();
    byte_count = h_uint8();
    output_address = h_uint16();
    output_value = h_uint16();
    exception_code = h_uint8();

    HParser *pdu_choice = h_indirect();
    HParser *modbus_tcp = h_sequence(transaction_id,
                                   protocol_id,
                                   length,
                                   unit_id,
                                   pdu_choice,
                                   NULL);

    HParser *pdu = h_sequence(function_code,
        h_choice(h_left(parse_read_request(), h_ch(0x01)),
                h_left(parse_read_request(), h_ch(0x02)),
                h_left(parse_read_request(), h_ch(0x03)),
                h_left(parse_read_request(), h_ch(0x04)),
                h_left(parse_write_single(), h_ch(0x05)),
                h_left(parse_write_single(), h_ch(0x06)),
                h_left(parse_write_multiple_request(), h_ch(0x0F)),
                h_left(parse_write_multiple_request(), h_ch(0x10)),
                h_left(parse_exception_response(), h_bits(8, false)),
                NULL),
        NULL);

    h_bind_indirect(pdu_choice, pdu);
    return modbus_tcp;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t *input = malloc(size);
    if (!input) {
        perror("Failed to allocate memory");
        fclose(f);
        return 1;
    }

    if (fread(input, 1, size, f) != size) {
        perror("Failed to read file");
        free(input);
        fclose(f);
        return 1;
    }

    HParser *modbus_parser = init_modbus_parser();
    HParseResult *result = h_parse(modbus_parser, input, size);

    if (!result) {
        fprintf(stderr, "Parse failed\n");
        free(input);
        fclose(f);
        return 1;
    }

    h_parse_result_free(result);
    free(input);
    fclose(f);
    return 0;
}