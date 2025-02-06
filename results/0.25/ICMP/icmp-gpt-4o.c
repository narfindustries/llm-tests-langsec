#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

// Define ICMP fields using Hammer
HParser *icmp_type() {
    return h_bits(8, false);
}

HParser *icmp_code() {
    return h_bits(8, false);
}

HParser *icmp_checksum() {
    return h_bits(16, false);
}

HParser *icmp_rest_of_header() {
    return h_bits(32, false);
}

// Define ICMP message parser
HParser *icmp_message() {
    return h_sequence(
        icmp_type(),
        icmp_code(),
        icmp_checksum(),
        icmp_rest_of_header(),
        h_end_p(), // Ensure the parser consumes the entire input
        NULL
    );
}

// Function to print ICMP message
void print_icmp_message(const HParsedToken *parsed) {
    if (!parsed) {
        printf("Failed to parse ICMP message.\n");
        return;
    }

    uint8_t type = parsed->seq->elements[0]->uint;
    uint8_t code = parsed->seq->elements[1]->uint;
    uint16_t checksum = parsed->seq->elements[2]->uint;
    uint32_t rest_of_header = parsed->seq->elements[3]->uint;

    printf("ICMP Message:\n");
    printf("Type: %u\n", type);
    printf("Code: %u\n", code);
    printf("Checksum: 0x%04x\n", checksum);
    printf("Rest of Header: 0x%08x\n", rest_of_header);
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

    HParseResult *result = h_parse(icmp_message(), buffer, file_size);
    if (result && result->ast) {
        print_icmp_message(result->ast);
    } else {
        fprintf(stderr, "Parsing failed.\n");
    }

    h_parse_result_free(result);
    free(buffer);

    return EXIT_SUCCESS;
}