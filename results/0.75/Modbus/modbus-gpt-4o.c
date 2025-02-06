#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

HParser *create_modbus_parser() {
    // Transaction Identifier (2 bytes)
    HParser *transaction_id = h_uint16();

    // Protocol Identifier (2 bytes, must be 0x0000 for Modbus)
    HParser *protocol_id = h_uint16();

    // Length (2 bytes)
    HParser *length = h_uint16();

    // Unit Identifier (1 byte)
    HParser *unit_id = h_uint8();

    // Function Code (1 byte)
    HParser *function_code = h_uint8();

    // Data length calculation (length field minus 2 bytes)
    HParser *fixed_minus_two = h_choice(h_value(h_uint16(), 2), NULL);
    HParser *data_length = h_calc_length(length, fixed_minus_two);

    // Data (variable length based on data_length)
    HParser *data = h_repeat_n(h_uint8(), data_length);

    // Modbus TCP frame parser (no CRC or LRC as they're not included in Modbus TCP)
    HParser *modbus_tcp_frame = h_sequence(
        transaction_id, protocol_id, length, unit_id, function_code, data, NULL);

    return modbus_tcp_frame;
}

void print_parse_tree(const HParseResult *result) {
    if (!result) {
        printf("Failed to parse!\n");
        return;
    }

    const HParsedToken *root = result->ast;
    
    printf("Transaction ID: %u\n", (unsigned int)root->seq->elements[0]->uint);
    printf("Protocol ID: %u\n", (unsigned int)root->seq->elements[1]->uint);
    printf("Length: %u\n", (unsigned int)root->seq->elements[2]->uint);
    printf("Unit ID: %u\n", (unsigned int)root->seq->elements[3]->uint);
    printf("Function Code: %u\n", (unsigned int)root->seq->elements[4]->uint);

    // Data bytes
    const HParsedToken *data = root->seq->elements[5];
    printf("Data: ");
    for (size_t i = 0; i < data->seq->used; i++) {
        printf("%02x ", data->seq->elements[i]->uint);
    }
    printf("\n");
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    size_t file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = (uint8_t *)malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = create_modbus_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    print_parse_tree(result);

    h_parse_result_free(result);
    free(buffer);

    return EXIT_SUCCESS;
}