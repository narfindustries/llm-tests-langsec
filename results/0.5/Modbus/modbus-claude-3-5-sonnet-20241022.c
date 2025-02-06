#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// Forward declarations
HParser* create_modbus_parser(void);
HParser* create_read_request(void);
HParser* create_read_response(const HParsedToken* byte_count, void* user_data);
HParser* create_single_write(void);
HParser* create_multiple_write_request(void);
HParser* create_multiple_write_response(void);
HParser* create_diagnostic_request(void);

// Action functions
HParsedToken* modbus_action(const HParseResult* p, void* user_data) {
    return (HParsedToken*)p;
}

HParser* init_modbus_parser(void) {
    HParser* transaction_id = h_uint16();
    HParser* protocol_id = h_uint16();
    HParser* length = h_uint16();
    HParser* unit_id = h_uint8();
    HParser* function_code = h_uint8();

    HParser* read_seq = h_sequence(transaction_id, protocol_id, length, unit_id, function_code, NULL);
    return h_action(read_seq, modbus_action, NULL);
}

HParser* create_read_request(void) {
    return h_sequence(h_uint16(), h_uint16(), NULL);
}

HParser* create_read_response(const HParsedToken* byte_count, void* user_data) {
    return h_repeat_n(h_uint8(), byte_count->uint);
}

HParser* create_single_write(void) {
    return h_sequence(h_uint16(), h_uint16(), NULL);
}

HParser* create_multiple_write_request(void) {
    HParser* addr = h_uint16();
    HParser* qty = h_uint16();
    HParser* byte_count = h_uint8();
    
    return h_sequence(addr, qty, byte_count, 
                     h_length_value(byte_count, h_uint8()), 
                     NULL);
}

HParser* create_multiple_write_response(void) {
    return h_sequence(h_uint16(), h_uint16(), NULL);
}

HParser* create_diagnostic_request(void) {
    return h_sequence(h_uint16(), h_uint16(), NULL);
}

HParser* create_modbus_parser(void) {
    HParser* header = h_sequence(
        h_uint16(),  // Transaction ID
        h_uint16(),  // Protocol ID
        h_uint16(),  // Length
        h_uint8(),   // Unit ID
        NULL
    );

    HParser* function_parser = h_choice(
        h_int_range(h_uint8(), 0x01, 0x04),  // Read functions
        h_int_range(h_uint8(), 0x05, 0x06),  // Single write
        h_int_range(h_uint8(), 0x0F, 0x10),  // Multiple write
        h_ch(0x08),                          // Diagnostics
        h_int_range(h_uint8(), 0x81, 0x98),  // Error responses
        NULL
    );

    return h_sequence(header, function_parser, NULL);
}

void print_parsed_data(const HParsedToken* token) {
    if (!token) return;
    
    switch (token->token_type) {
        case TT_SEQUENCE:
            for (size_t i = 0; i < token->seq->used; i++) {
                print_parsed_data(token->seq->elements[i]);
            }
            break;
        case TT_UINT:
            printf("0x%X ", (unsigned int)token->uint);
            break;
        case TT_BYTES:
            for (size_t i = 0; i < token->bytes.len; i++) {
                printf("%02X ", token->bytes.token[i]);
            }
            break;
        default:
            printf("Unknown token type\n");
    }
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* input = malloc(size);
    if (!input) {
        fclose(file);
        fprintf(stderr, "Memory allocation failed\n");
        return 1;
    }

    if (fread(input, 1, size, file) != size) {
        free(input);
        fclose(file);
        fprintf(stderr, "Failed to read file\n");
        return 1;
    }

    HParser* parser = create_modbus_parser();
    if (!parser) {
        free(input);
        fclose(file);
        fprintf(stderr, "Failed to create parser\n");
        return 1;
    }

    HParseResult* parse_result = h_parse(parser, input, size);
    if (parse_result && parse_result->ast) {
        printf("Parsed Modbus message:\n");
        print_parsed_data(parse_result->ast);
        printf("\n");
        h_parse_result_free(parse_result);
    } else {
        fprintf(stderr, "Parsing failed\n");
    }

    free(input);
    fclose(file);
    return parse_result ? 0 : 1;
}