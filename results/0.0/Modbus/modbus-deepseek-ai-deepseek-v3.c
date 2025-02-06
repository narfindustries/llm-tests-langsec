#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// Define parsers for basic Modbus fields
HParser *modbus_slave_address_parser() {
    return h_uint8();
}

HParser *modbus_function_code_parser() {
    return h_uint8();
}

HParser *modbus_data_parser() {
    return h_many(h_uint8());
}

HParser *modbus_error_check_parser() {
    return h_sequence(h_uint8(), h_uint8(), NULL);
}

HParser *modbus_transaction_id_parser() {
    return h_uint16();
}

HParser *modbus_protocol_id_parser() {
    return h_uint16();
}

HParser *modbus_length_parser() {
    return h_uint16();
}

HParser *modbus_unit_id_parser() {
    return h_uint8();
}

HParser *modbus_exception_code_parser() {
    return h_uint8();
}

HParser *modbus_mei_type_parser() {
    return h_uint8();
}

// Define parser for Modbus PDU
HParser *modbus_pdu_parser() {
    return h_sequence(
        modbus_function_code_parser(),
        modbus_data_parser(),
        NULL
    );
}

// Define parser for Modbus ADU (RTU/ASCII)
HParser *modbus_adu_parser() {
    return h_sequence(
        modbus_slave_address_parser(),
        modbus_pdu_parser(),
        modbus_error_check_parser(),
        NULL
    );
}

// Define parser for Modbus TCP/IP ADU
HParser *modbus_tcp_adu_parser() {
    return h_sequence(
        modbus_transaction_id_parser(),
        modbus_protocol_id_parser(),
        modbus_length_parser(),
        modbus_unit_id_parser(),
        modbus_pdu_parser(),
        NULL
    );
}

// Define parser for Modbus Exception Response
HParser *modbus_exception_response_parser() {
    return h_sequence(
        modbus_function_code_parser(),
        modbus_exception_code_parser(),
        NULL
    );
}

// Define parser for Modbus Diagnostic Function Codes
HParser *modbus_diagnostic_parser() {
    return h_sequence(
        modbus_function_code_parser(),
        modbus_data_parser(),
        NULL
    );
}

// Define parser for Modbus Encapsulated Interface Transport
HParser *modbus_encapsulated_parser() {
    return h_sequence(
        modbus_function_code_parser(),
        modbus_mei_type_parser(),
        modbus_data_parser(),
        NULL
    );
}

// Main function to parse Modbus binary file
int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
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

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = modbus_tcp_adu_parser(); // Change to modbus_adu_parser() for RTU/ASCII
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("Parsing successful!\n");
    } else {
        printf("Parsing failed!\n");
    }

    free(buffer);
    return 0;
}