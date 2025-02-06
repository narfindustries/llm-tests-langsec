#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Function to create parsers for Modbus fields
HParser *create_parsers() {
    // Define parsers for Modbus fields
    HParser *slave_address = h_uint8();
    HParser *function_code = h_uint8();
    HParser *crc = h_int_range(h_uint16(), 0x0000, 0xFFFF);
    HParser *transaction_id = h_int_range(h_uint16(), 0x0000, 0xFFFF);
    HParser *protocol_id = h_int_range(h_uint16(), 0x0000, 0xFFFF);
    HParser *length = h_int_range(h_uint16(), 0x0000, 0xFFFF);
    HParser *unit_id = h_uint8();

    // Define parser for Modbus RTU ADU
    HParser *modbus_rtu_adu = h_sequence(slave_address, function_code, h_many1(h_uint8()), crc, NULL);

    // Define parser for Modbus TCP ADU
    HParser *modbus_tcp_adu = h_sequence(transaction_id, protocol_id, length, unit_id, function_code, h_many1(h_uint8()), NULL);

    // Define a choice parser to select between RTU and TCP
    return h_choice(modbus_tcp_adu, modbus_rtu_adu, NULL);
}

void parse_modbus_message(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }
    
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    unsigned char *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate buffer");
        fclose(file);
        exit(EXIT_FAILURE);
    }
    
    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *modbus_adu = create_parsers();
    HParseResult *result = h_parse(modbus_adu, buffer, file_size);
    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed.\n");
    }

    h_parser_free(modbus_adu);
    free(buffer);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return EXIT_FAILURE;
    }
    
    parse_modbus_message(argv[1]);
    return EXIT_SUCCESS;
}