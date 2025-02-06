#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define Modbus constants
#define MODBUS_TCP_PROTOCOL_ID 0

// Function prototypes
static void parse_modbus_tcp(const uint8_t *data, size_t length);
static void print_hex(const uint8_t *data, size_t length);

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <modbus_binary_file>\n", argv[0]);
        return 1;
    }

    const char *filename = argv[1];
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    // Get file size
    fseek(file, 0, SEEK_END);
    long filesize = ftell(file);
    fseek(file, 0, SEEK_SET);

    // Read file into memory
    uint8_t *buffer = malloc(filesize);
    if (!buffer) {
        fprintf(stderr, "Memory allocation failed\n");
        fclose(file);
        return 1;
    }

    size_t read_bytes = fread(buffer, 1, filesize, file);
    if (read_bytes != filesize) {
        fprintf(stderr, "Failed to read the file\n");
        free(buffer);
        fclose(file);
        return 1;
    }

    // Parse the Modbus TCP data
    parse_modbus_tcp(buffer, filesize);

    // Cleanup
    free(buffer);
    fclose(file);

    return 0;
}

static void parse_modbus_tcp(const uint8_t *data, size_t length) {
    HParser *transaction_id = h_uint16();
    HParser *protocol_id = h_uint16();
    HParser *length_field = h_uint16();
    HParser *unit_id = h_uint8();
    HParser *function_code = h_uint8();
    HParser *data_field = h_bytes(length - 7); // Length minus headers and function code

    HParser *modbus_tcp_frame = h_sequence(transaction_id, protocol_id, length_field, unit_id, function_code, data_field, NULL);

    HParseResult *result = h_parse(modbus_tcp_frame, data, length);
    if (result) {
        printf("Modbus TCP Frame Parsed Successfully:\n");
        printf("Transaction ID: %u\n", *(uint16_t *)h_value_uint(result->ast->children[0]));
        printf("Protocol ID: %u\n", *(uint16_t *)h_value_uint(result->ast->children[1]));
        printf("Length: %u\n", *(uint16_t *)h_value_uint(result->ast->children[2]));
        printf("Unit ID: %u\n", *(uint8_t *)h_value_uint(result->ast->children[3]));
        printf("Function Code: %u\n", *(uint8_t *)h_value_uint(result->ast->children[4]));
        printf("Data: ");
        print_hex(h_blob_value(result->ast->children[5]), h_blob_size(result->ast->children[5]));
        printf("\n");
    } else {
        fprintf(stderr, "Failed to parse Modbus TCP frame\n");
    }

    h_parse_result_free(result);
    h_parser_free(modbus_tcp_frame);
}

static void print_hex(const uint8_t *data, size_t length) {
    for (size_t i = 0; i < length; i++) {
        printf("%02X ", data[i]);
    }
}