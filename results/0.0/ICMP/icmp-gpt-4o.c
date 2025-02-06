#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

// Define ICMP fields using Hammer
HParser *icmp_type() {
    return h_uint8();
}

HParser *icmp_code() {
    return h_uint8();
}

HParser *icmp_checksum() {
    return h_uint16();
}

HParser *icmp_rest_of_header() {
    return h_uint32();
}

HParser *icmp_data() {
    return h_many(h_uint8());
}

// Define the ICMP message parser
HParser *icmp_message() {
    return h_sequence(icmp_type(), icmp_code(), icmp_checksum(), icmp_rest_of_header(), icmp_data(), NULL);
}

// Function to print ICMP message
void print_icmp_message(const uint8_t *data, size_t length) {
    HParseResult *result = h_parse(icmp_message(), data, length);
    if (result) {
        const HParsedToken *seq = result->ast;
        uint8_t type = seq->seq->elements[0]->uint;
        uint8_t code = seq->seq->elements[1]->uint;
        uint16_t checksum = seq->seq->elements[2]->uint;
        uint32_t rest_of_header = seq->seq->elements[3]->uint;

        printf("ICMP Message:\n");
        printf("Type: %u\n", type);
        printf("Code: %u\n", code);
        printf("Checksum: 0x%04x\n", checksum);
        printf("Rest of Header: 0x%08x\n", rest_of_header);

        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse ICMP message.\n");
    }
}

// Main function to read binary file and parse ICMP message
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
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    print_icmp_message(buffer, file_size);

    free(buffer);
    return EXIT_SUCCESS;
}