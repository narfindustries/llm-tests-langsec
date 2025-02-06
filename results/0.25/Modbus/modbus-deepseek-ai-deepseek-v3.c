#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct {
    uint8_t function_code;
    uint16_t starting_address;
    uint16_t quantity;
    uint8_t exception_code;
    uint16_t sub_function_code;
    uint8_t reference_type;
    uint16_t file_number;
    uint16_t record_number;
    uint16_t record_length;
    uint16_t fifo_count;
    uint8_t object_id;
    uint8_t unit_id;
    uint16_t transaction_id;
    uint16_t protocol_id;
    uint16_t length;
} ModbusPDU;

HParser* parse_modbus_pdu() {
    return h_sequence(
        h_uint8(), // Function Code
        h_choice(
            h_sequence( // Read Coils, Read Discrete Inputs, Read Holding Registers, Read Input Registers
                h_uint16(),
                h_uint16(),
                NULL
            ),
            h_sequence( // Write Single Coil, Write Single Register
                h_uint16(),
                h_uint16(),
                NULL
            ),
            h_sequence( // Write Multiple Coils, Write Multiple Registers
                h_uint16(),
                h_uint16(),
                h_uint8(),
                h_repeat_n(h_uint8(), h_uint8()),
                NULL
            ),
            h_sequence( // Read Exception Status
                h_uint8(),
                NULL
            ),
            h_sequence( // Diagnostics
                h_uint16(),
                NULL
            ),
            h_sequence( // Get Comm Event Counter, Get Comm Event Log
                h_uint8(),
                NULL
            ),
            h_sequence( // Report Slave ID
                h_uint8(),
                NULL
            ),
            h_sequence( // Read File Record, Write File Record
                h_uint8(),
                h_uint16(),
                h_uint16(),
                h_uint16(),
                NULL
            ),
            h_sequence( // Mask Write Register
                h_uint16(),
                h_uint16(),
                h_uint16(),
                NULL
            ),
            h_sequence( // Read/Write Multiple Registers
                h_uint16(),
                h_uint16(),
                h_uint16(),
                h_uint16(),
                h_uint8(),
                h_repeat_n(h_uint8(), h_uint8()),
                NULL
            ),
            h_sequence( // Read FIFO Queue
                h_uint16(),
                NULL
            ),
            h_sequence( // Encapsulated Interface Transport
                h_uint8(),
                h_uint8(),
                NULL
            ),
            NULL
        ),
        NULL
    );
}

HParser* parse_modbus_rtu() {
    return h_sequence(
        h_uint8(), // Slave Address
        parse_modbus_pdu(),
        h_uint16(), // CRC
        NULL
    );
}

HParser* parse_modbus_tcp() {
    return h_sequence(
        h_uint16(), // Transaction Identifier
        h_uint16(), // Protocol Identifier
        h_uint16(), // Length
        h_uint8(), // Unit Identifier
        parse_modbus_pdu(),
        NULL
    );
}

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

    uint8_t *buffer = (uint8_t*)malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = parse_modbus_tcp();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }

    free(buffer);
    return 0;
}