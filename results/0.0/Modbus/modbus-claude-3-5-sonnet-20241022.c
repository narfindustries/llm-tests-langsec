#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Parser declarations
HParser *mbap_transaction_id;
HParser *mbap_protocol_id;
HParser *mbap_length;
HParser *mbap_unit_id;
HParser *function_code;
HParser *starting_address;
HParser *quantity;
HParser *output_address;
HParser *output_value;
HParser *byte_count;
HParser *exception_code;

// Helper function to create data field parser based on byte count
HParser* create_data_field_parser(void) {
    return h_length_value(byte_count, h_uint8());
}

// Function-specific request parsers
HParser* create_read_request_parser(void) {
    return h_sequence(starting_address, quantity, NULL);
}

HParser* create_write_single_request_parser(void) {
    return h_sequence(output_address, output_value, NULL);
}

HParser* create_write_multiple_request_parser(void) {
    return h_sequence(starting_address, quantity, byte_count, create_data_field_parser(), NULL);
}

// Error response parser
HParser* create_error_response_parser(void) {
    return h_sequence(
        h_ch(0x80),
        exception_code,
        NULL
    );
}

// Main Modbus TCP parser
HParser* create_modbus_tcp_parser(void) {
    // Initialize parsers
    mbap_transaction_id = h_uint16();
    mbap_protocol_id = h_uint16();
    mbap_length = h_uint16();
    mbap_unit_id = h_uint8();
    function_code = h_uint8();
    starting_address = h_uint16();
    quantity = h_uint16();
    output_address = h_uint16();
    output_value = h_uint16();
    byte_count = h_uint8();
    exception_code = h_uint8();

    // MBAP Header
    HParser *mbap = h_sequence(
        mbap_transaction_id,
        mbap_protocol_id,
        mbap_length,
        mbap_unit_id,
        NULL
    );

    // PDU
    HParser *pdu = h_choice(
        create_read_request_parser(),
        create_write_single_request_parser(),
        create_write_multiple_request_parser(),
        create_error_response_parser(),
        NULL
    );

    return h_sequence(mbap, pdu, NULL);
}

void print_parse_result(HParseResult *result) {
    if (!result) {
        printf("Parsing failed\n");
        return;
    }
    
    printf("Parsing successful\n");
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *input = malloc(file_size);
    if (!input) {
        perror("Memory allocation failed");
        fclose(file);
        return 1;
    }

    size_t bytes_read = fread(input, 1, file_size, file);
    fclose(file);

    if (bytes_read != file_size) {
        fprintf(stderr, "Failed to read entire file\n");
        free(input);
        return 1;
    }

    HParser *modbus_parser = create_modbus_tcp_parser();
    HParseResult *result = h_parse(modbus_parser, input, file_size);
    
    print_parse_result(result);

    if (result) {
        h_parse_result_free(result);
    }
    free(input);

    return 0;
}