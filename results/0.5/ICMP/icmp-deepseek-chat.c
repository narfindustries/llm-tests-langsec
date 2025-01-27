#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define the ICMP header structure
typedef struct {
    uint8_t type;
    uint8_t code;
    uint16_t checksum;
    uint16_t identifier;
    uint16_t sequence_number;
} icmp_header_t;

// Define the Hammer parser for the ICMP header
HParser *icmp_header_parser() {
    return h_sequence(
        h_uint8(),  // type
        h_uint8(),  // code
        h_uint16(), // checksum
        h_uint16(), // identifier
        h_uint16(), // sequence_number
        NULL
    );
}

// Function to parse ICMP header from a buffer
icmp_header_t *parse_icmp_header(const uint8_t *data, size_t length) {
    HParser *parser = icmp_header_parser();
    HParseResult *result = h_parse(parser, data, length);

    if (!result) {
        fprintf(stderr, "Failed to parse ICMP header\n");
        return NULL;
    }

    icmp_header_t *header = (icmp_header_t *)malloc(sizeof(icmp_header_t));
    if (!header) {
        fprintf(stderr, "Memory allocation failed\n");
        h_parse_result_free(result);
        return NULL;
    }

    HValue *value = result->ast;
    header->type = h_value_get_uint8(h_sequence_index(value, 0));
    header->code = h_value_get_uint8(h_sequence_index(value, 1));
    header->checksum = h_value_get_uint16(h_sequence_index(value, 2));
    header->identifier = h_value_get_uint16(h_sequence_index(value, 3));
    header->sequence_number = h_value_get_uint16(h_sequence_index(value, 4));

    h_parse_result_free(result);
    return header;
}

// Function to free the ICMP header
void free_icmp_header(icmp_header_t *header) {
    if (header) {
        free(header);
    }
}

// Example usage
int main() {
    uint8_t icmp_data[] = {
        0x08, 0x00, 0x4d, 0x4a, 0x00, 0x01, 0x00, 0x02
    };
    size_t icmp_length = sizeof(icmp_data);

    icmp_header_t *header = parse_icmp_header(icmp_data, icmp_length);
    if (header) {
        printf("Type: %d\n", header->type);
        printf("Code: %d\n", header->code);
        printf("Checksum: %d\n", header->checksum);
        printf("Identifier: %d\n", header->identifier);
        printf("Sequence Number: %d\n", header->sequence_number);
        free_icmp_header(header);
    }

    return 0;
}