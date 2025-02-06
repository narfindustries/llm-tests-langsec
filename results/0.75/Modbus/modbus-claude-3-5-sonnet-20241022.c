#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// MBAP Header parsers
static HParser* transaction_id;
static HParser* protocol_id;
static HParser* length;
static HParser* unit_id;

// Function code parser
static HParser* function_code;

// Data parsers
static HParser* starting_address;
static HParser* quantity;
static HParser* output_value;
static HParser* byte_count;

// Error response parser
static HParser* error_code;

void init_parsers() {
    transaction_id = h_uint16();
    protocol_id = h_uint16();
    length = h_uint16();
    unit_id = h_uint8();
    function_code = h_uint8();
    starting_address = h_uint16();
    quantity = h_uint16();
    output_value = h_uint16();
    byte_count = h_uint8();
    error_code = h_uint8();
}

HParsedToken* act_noop(const HParseResult* p, void* user_data) {
    return (HParsedToken*)p->ast;
}

static HParser* create_modbus_read_request() {
    return h_sequence(starting_address, quantity, NULL);
}

static HParser* create_modbus_write_single() {
    return h_sequence(starting_address, output_value, NULL);
}

static HParser* create_modbus_write_multiple() {
    return h_sequence(starting_address, 
                     quantity,
                     byte_count,
                     h_length_value(byte_count, h_uint8()),
                     NULL);
}

static HParser* create_modbus_error_response() {
    return h_sequence(error_code, NULL);
}

static HParser* create_modbus_pdu() {
    return h_choice(h_action(h_sequence(h_ch(0x01), create_modbus_read_request(), NULL), act_noop, NULL),
                   h_action(h_sequence(h_ch(0x02), create_modbus_read_request(), NULL), act_noop, NULL),
                   h_action(h_sequence(h_ch(0x03), create_modbus_read_request(), NULL), act_noop, NULL),
                   h_action(h_sequence(h_ch(0x04), create_modbus_read_request(), NULL), act_noop, NULL),
                   h_action(h_sequence(h_ch(0x05), create_modbus_write_single(), NULL), act_noop, NULL),
                   h_action(h_sequence(h_ch(0x06), create_modbus_write_single(), NULL), act_noop, NULL),
                   h_action(h_sequence(h_ch(0x0F), create_modbus_write_multiple(), NULL), act_noop, NULL),
                   h_action(h_sequence(h_ch(0x10), create_modbus_write_multiple(), NULL), act_noop, NULL),
                   h_action(h_sequence(h_int_range(h_uint8(), 0x80, 0xFF), create_modbus_error_response(), NULL), act_noop, NULL),
                   NULL);
}

static HParser* create_modbus_parser() {
    return h_sequence(transaction_id,
                     protocol_id,
                     length,
                     unit_id,
                     create_modbus_pdu(),
                     NULL);
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        fprintf(stderr, "Could not open file: %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* input = malloc(file_size);
    if (!input) {
        fclose(file);
        fprintf(stderr, "Memory allocation failed\n");
        return 1;
    }

    size_t bytes_read = fread(input, 1, file_size, file);
    fclose(file);

    if (bytes_read != file_size) {
        free(input);
        fprintf(stderr, "File read error\n");
        return 1;
    }

    init_parsers();
    HParser* modbus_parser = create_modbus_parser();
    HParseResult* result = h_parse(modbus_parser, input, file_size);

    if (result) {
        printf("Parsing successful\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed\n");
    }

    free(input);
    return 0;
}