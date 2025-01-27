#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// ICMP Header Parser
HParser *icmp_parser() {
    return h_sequence(
        h_uint8(),  // Type
        h_uint8(),  // Code
        h_uint16(), // Checksum
        h_uint16(), // Identifier (optional)
        h_uint16(), // Sequence Number (optional)
        h_many(h_uint8()) // Data (optional)
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

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = icmp_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (!result) {
        fprintf(stderr, "Failed to parse ICMP packet\n");
        free(buffer);
        return 1;
    }

    printf("ICMP Packet Parsed Successfully\n");

    h_parse_result_free(result);
    free(buffer);
    return 0;
}