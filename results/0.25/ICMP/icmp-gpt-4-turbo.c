#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define ICMP types
#define ECHO_REPLY 0
#define DEST_UNREACHABLE 3
#define SOURCE_QUENCH 4
#define REDIRECT 5
#define ECHO_REQUEST 8
#define ROUTER_ADVERTISEMENT 9
#define ROUTER_SOLICITATION 10
#define TIME_EXCEEDED 11
#define PARAMETER_PROBLEM 12
#define TIMESTAMP 13
#define TIMESTAMP_REPLY 14
#define INFORMATION_REQUEST 15
#define INFORMATION_REPLY 16
#define ADDRESS_MASK_REQUEST 17
#define ADDRESS_MASK_REPLY 18

// Parser for ICMP message
static HParser *icmp_parser() {
    HParser *type = h_uint8();
    HParser *code = h_uint8();
    HParser *checksum = h_uint16();

    HParser *rest_of_header = h_bits(32, false); // Generic 32 bits for simplicity

    return h_sequence(type, code, checksum, rest_of_header, NULL);
}

// Main function
int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <ICMP binary file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
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

    if (fread(data, 1, file_size, file) != file_size) {
        fprintf(stderr, "Failed to read the file\n");
        free(data);
        fclose(file);
        return 1;
    }

    // Create the parser
    HParser *parser = icmp_parser();
    HParseResult *result = h_parse(parser, data, file_size);

    if (result) {
        printf("ICMP packet parsed successfully.\n");
        // You can add code here to handle the parsed data
    } else {
        printf("Failed to parse ICMP packet.\n");
    }

    // Cleanup
    h_parse_result_free(result);
    h_free_parser(parser);
    free(data);
    fclose(file);

    return 0;
}