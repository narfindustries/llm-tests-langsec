#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Function to parse ICMP header fields
HParser *icmp_type_parser() {
    return h_uint8();
}

HParser *icmp_code_parser() {
    return h_uint8();
}

HParser *icmp_checksum_parser() {
    return h_uint16();
}

// Function to parse the rest of the header, which could vary by type
HParser *icmp_rest_of_header_parser() {
    return h_sequence(
        h_uint16(), // Identifier (16 bits)
        h_uint16(), // Sequence Number (16 bits)
        h_data(4),  // Optional data or additional fields as needed
        NULL
    );
}

// Build the ICMP message parser
HParser *icmp_message_parser() {
    return h_sequence(
        icmp_type_parser(),
        icmp_code_parser(),
        icmp_checksum_parser(),
        icmp_rest_of_header_parser(),
        NULL
    );
}

// Main function that takes binary file input and parses it
int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    const char *filename = argv[1];
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    // Move to the end of the file to get its size
    fseek(file, 0, SEEK_END);
    size_t file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    // Read the file content into a buffer
    unsigned char *buffer = (unsigned char *)malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate buffer");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    // Parse the buffer
    HParseResult *result = h_parse(icmp_message_parser(), buffer, file_size);
    if (result) {
        printf("ICMP message parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse ICMP message.\n");
    }

    free(buffer);
    return EXIT_SUCCESS;
}