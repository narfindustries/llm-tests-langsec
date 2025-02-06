#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define parsers for ICMP fields
static HParser *icmp_type;
static HParser *icmp_code;
static HParser *icmp_checksum;
static HParser *icmp_rest_of_header;
static HParser *icmp_message;

void init_parsers() {
    icmp_type = h_uint8();
    icmp_code = h_uint8();
    icmp_checksum = h_uint16();
    icmp_rest_of_header = h_bits(32, false); // Generic 32 bits for simplicity
    icmp_message = h_sequence(icmp_type, icmp_code, icmp_checksum, icmp_rest_of_header, NULL);
}

void parse_icmp(const uint8_t *data, size_t length) {
    HParseResult *result = h_parse(icmp_message, data, length);
    if (result) {
        printf("ICMP Packet Parsed Successfully:\n");
        printf("Type: %d\n", *(uint8_t*)h_value_uint(result->ast->children[0]));
        printf("Code: %d\n", *(uint8_t*)h_value_uint(result->ast->children[1]));
        printf("Checksum: %d\n", *(uint16_t*)h_value_uint(result->ast->children[2]));
        // Rest of Header is not interpreted further here
    } else {
        printf("Failed to parse ICMP packet.\n");
    }
    h_parse_result_free(result);
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <icmp_binary_file>\n", argv[0]);
        return 1;
    }

    const char *filename = argv[1];
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    // Seek to the end of the file to determine the file size
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    // Read the entire file into memory
    uint8_t *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    size_t read_size = fread(data, 1, file_size, file);
    if (read_size != file_size) {
        fprintf(stderr, "Error reading file\n");
        free(data);
        fclose(file);
        return 1;
    }

    // Initialize parsers
    init_parsers();

    // Parse the ICMP packet
    parse_icmp(data, file_size);

    // Cleanup
    free(data);
    fclose(file);

    return 0;
}