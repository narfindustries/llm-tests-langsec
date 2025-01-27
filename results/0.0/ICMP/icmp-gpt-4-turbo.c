#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// ICMP Type and Code
#define ICMP_ECHO_REPLY 0
#define ICMP_DEST_UNREACH 3
#define ICMP_SOURCE_QUENCH 4
#define ICMP_REDIRECT 5
#define ICMP_ECHO_REQUEST 8
#define ICMP_TIME_EXCEEDED 11
#define ICMP_PARAMETER_PROBLEM 12
#define ICMP_TIMESTAMP_REQUEST 13
#define ICMP_TIMESTAMP_REPLY 14
#define ICMP_INFO_REQUEST 15
#define ICMP_INFO_REPLY 16

// ICMP Header structure
typedef struct {
    uint8_t type;
    uint8_t code;
    uint16_t checksum;
    uint32_t rest_of_header;
} icmp_header;

// Parsing functions
HParser *icmp_type;
HParser *icmp_code;
HParser *icmp_checksum;
HParser *icmp_rest_of_header;
HParser *icmp_packet;

void init_parsers() {
    icmp_type = h_uint8();
    icmp_code = h_uint8();
    icmp_checksum = h_uint16();
    icmp_rest_of_header = h_uint32();
    icmp_packet = h_sequence(icmp_type, icmp_code, icmp_checksum, icmp_rest_of_header, NULL);
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <ICMP packet file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t length = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(length);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if (fread(data, 1, length, file) != length) {
        fprintf(stderr, "Failed to read file\n");
        free(data);
        fclose(file);
        return 1;
    }

    fclose(file);

    init_parsers();

    HParseResult *result = h_parse(icmp_packet, data, length);
    if (result) {
        icmp_header *hdr = (icmp_header *)result->ast;
        printf("ICMP Type: %d\n", hdr->type);
        printf("ICMP Code: %d\n", hdr->code);
        printf("ICMP Checksum: %u\n", hdr->checksum);
        printf("Rest of Header: %u\n", hdr->rest_of_header);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse ICMP packet\n");
    }

    free(data);
    return 0;
}