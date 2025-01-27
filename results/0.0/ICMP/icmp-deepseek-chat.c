#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// ICMP Header structure
typedef struct {
    uint8_t type;
    uint8_t code;
    uint16_t checksum;
    union {
        struct {
            uint16_t id;
            uint16_t sequence;
        } echo;
        uint32_t gateway;
        struct {
            uint16_t unused;
            uint16_t mtu;
        } frag;
    } un;
} ICMPHeader;

// Parser for ICMP Header
HParser *icmp_header_parser() {
    return h_sequence(
        h_uint8(),  // type
        h_uint8(),  // code
        h_uint16(), // checksum
        h_choice(
            h_sequence(h_uint16(), h_uint16(), NULL), // echo request/reply
            h_uint32(), // gateway
            h_sequence(h_uint16(), h_uint16(), NULL), // fragmentation needed
            NULL
        ),
        NULL
    );
}

// Main function
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

    HParser *parser = icmp_header_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (!result) {
        fprintf(stderr, "Failed to parse ICMP header\n");
        free(buffer);
        return 1;
    }

    ICMPHeader *header = (ICMPHeader *)result->ast;
    printf("Type: %u\n", header->type);
    printf("Code: %u\n", header->code);
    printf("Checksum: %u\n", header->checksum);

    if (header->type == 0 || header->type == 8) {
        printf("ID: %u\n", header->un.echo.id);
        printf("Sequence: %u\n", header->un.echo.sequence);
    } else if (header->type == 5) {
        printf("Gateway: %u\n", header->un.gateway);
    } else if (header->type == 3 && header->code == 4) {
        printf("Unused: %u\n", header->un.frag.unused);
        printf("MTU: %u\n", header->un.frag.mtu);
    }

    h_parse_result_free(result);
    free(buffer);

    return 0;
}