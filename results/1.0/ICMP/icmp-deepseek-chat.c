#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct {
    uint8_t type;
    uint8_t code;
    uint16_t checksum;
    uint16_t identifier;
    uint16_t sequence_number;
} ICMPHeader;

HParser *icmp_parser() {
    return h_sequence(
        h_uint8(&ICMPHeader.type),
        h_uint8(&ICMPHeader.code),
        h_uint16(&ICMPHeader.checksum),
        h_uint16(&ICMPHeader.identifier),
        h_uint16(&ICMPHeader.sequence_number),
        NULL
    );
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
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

    uint8_t *buffer = (uint8_t *)malloc(file_size);
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
        fprintf(stderr, "Parsing failed\n");
        free(buffer);
        return 1;
    }

    ICMPHeader *icmp_header = (ICMPHeader *)result->ast;
    printf("Type: %u\n", icmp_header->type);
    printf("Code: %u\n", icmp_header->code);
    printf("Checksum: %u\n", icmp_header->checksum);
    printf("Identifier: %u\n", icmp_header->identifier);
    printf("Sequence Number: %u\n", icmp_header->sequence_number);

    h_parse_result_free(result);
    free(buffer);

    return 0;
}