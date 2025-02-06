#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

// Define parsers for fixed fields
HParser *create_parsers() {
    HParser *slave_address = h_uint8();
    HParser *function_code = h_uint8();
    HParser *transaction_id = h_uint16();
    HParser *protocol_id = h_uint16();
    HParser *length_field = h_uint16();
    HParser *unit_id = h_uint8();

    // Define parsers for data and error-checking fields
    HParser *data_field = h_length_value(h_uint16(), h_uint8());
    HParser *crc = h_uint16();
    HParser *lrc = h_uint8();

    HParser *modbus_tcp_header = h_sequence(transaction_id, protocol_id, length_field, unit_id, NULL);
    HParser *modbus_frame = h_choice(
        h_sequence(slave_address, function_code, data_field, crc, NULL),  // Modbus RTU/ASCII Frame
        h_sequence(modbus_tcp_header, function_code, data_field, NULL)    // Modbus TCP Frame
    );

    return modbus_frame;
}

void handle_parsed_data(const HParsedToken *token) {
    if (!token) {
        fprintf(stderr, "Error: Invalid token.\n");
        return;
    }

    // Extract and print fields.
    printf("Parsed Modbus Frame:\n");
    if (token->seq->used == 4) { // Modbus TCP Frame
        printf("Transaction ID: %u\n", token->seq->elements[0]->uint);
        printf("Protocol ID: %u\n", token->seq->elements[1]->uint);
        printf("Length: %u\n", token->seq->elements[2]->uint);
        printf("Unit ID: %u\n", token->seq->elements[3]->uint);
    } else { // Modbus RTU/ASCII Frame
        printf("Slave Address: %u\n", token->seq->elements[0]->uint);
    }
    printf("Function Code: %u\n", token->seq->elements[1]->uint);
    printf("Data Length: %lu\n", token->seq->elements[2]->bytes.len);
    printf("CRC/LRC: %u\n", token->seq->elements[3]->uint);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    // Open the binary file
    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Unable to open file");
        return EXIT_FAILURE;
    }

    // Get file size
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    // Read file data
    uint8_t *data = malloc(file_size);
    if (!data) {
        fprintf(stderr, "Memory allocation failed\n");
        fclose(file);
        return EXIT_FAILURE;
    }
    fread(data, 1, file_size, file);
    fclose(file);

    HParser *modbus_frame = create_parsers();

    // Parse the data
    HParseResult *result = h_parse(modbus_frame, data, file_size);
    if (result->ast) {
        handle_parsed_data(result->ast);
    } else {
        fprintf(stderr, "Parsing failed\n");
    }

    // Clean up
    h_parse_result_free(result);
    free(data);

    return EXIT_SUCCESS;
}