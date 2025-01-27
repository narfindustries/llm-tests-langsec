#include <hammer/hammer.h>

/* Define the ICMP header structure */
typedef struct {
    uint8_t type;
    uint8_t code;
    uint16_t checksum;
    union {
        struct {
            uint16_t identifier;
            uint16_t sequence_number;
        } echo; /* Echo Request/Reply */
        uint32_t gateway; /* Redirect */
        struct {
            uint16_t unused;
            uint16_t mtu;
        } frag; /* Fragmentation Needed */
    } rest_of_header;
} icmp_header_t;

/* Define parsers for each field */
HParser *type_parser = h_bits(8, true);
HParser *code_parser = h_bits(8, true);
HParser *checksum_parser = h_bits(16, true);

HParser *identifier_parser = h_bits(16, true);
HParser *sequence_number_parser = h_bits(16, true);

HParser *gateway_parser = h_bits(32, true);

HParser *unused_parser = h_bits(16, true);
HParser *mtu_parser = h_bits(16, true);

/* Define parsers for ICMP message types */
HParser *echo_parser = h_sequence(identifier_parser, sequence_number_parser, NULL);
HParser *redirect_parser = gateway_parser;
HParser *frag_parser = h_sequence(unused_parser, mtu_parser, NULL);

/* Define a choice parser for the rest_of_header field */
HParser *rest_of_header_parser = h_choice(
    h_if(h_eq(type_parser, h_uint8_val(0)), echo_parser),    /* Echo Reply */
    h_if(h_eq(type_parser, h_uint8_val(8)), echo_parser),    /* Echo Request */
    h_if(h_eq(type_parser, h_uint8_val(5)), redirect_parser),/* Redirect */
    h_if(h_eq(type_parser, h_uint8_val(3)), frag_parser),    /* Destination Unreachable (Fragmentation Needed) */
    NULL
);

/* Define the complete ICMP header parser */
HParser *icmp_header_parser = h_sequence(
    type_parser,
    code_parser,
    checksum_parser,
    rest_of_header_parser,
    NULL
);

/* Function to parse ICMP packet */
icmp_header_t *parse_icmp_packet(const uint8_t *data, size_t length) {
    HParseResult *result = h_parse(icmp_header_parser, data, length);
    if (result) {
        icmp_header_t *header = malloc(sizeof(icmp_header_t));
        header->type = h_parse_result_uint8(result, 0);
        header->code = h_parse_result_uint8(result, 1);
        header->checksum = h_parse_result_uint16(result, 2);

        switch (header->type) {
            case 0: /* Echo Reply */
            case 8: /* Echo Request */
                header->rest_of_header.echo.identifier = h_parse_result_uint16(result, 3);
                header->rest_of_header.echo.sequence_number = h_parse_result_uint16(result, 4);
                break;
            case 5: /* Redirect */
                header->rest_of_header.gateway = h_parse_result_uint32(result, 3);
                break;
            case 3: /* Destination Unreachable (Fragmentation Needed) */
                header->rest_of_header.frag.unused = h_parse_result_uint16(result, 3);
                header->rest_of_header.frag.mtu = h_parse_result_uint16(result, 4);
                break;
            default:
                /* Handle other types if necessary */
                break;
        }

        h_parse_result_free(result);
        return header;
    }
    return NULL;
}

int main() {
    /* Example usage */
    uint8_t icmp_data[] = {8, 0, 0x3a, 0x7c, 0x12, 0x34, 0x00, 0x01}; /* ICMP Echo Request */
    size_t length = sizeof(icmp_data);

    icmp_header_t *header = parse_icmp_packet(icmp_data, length);
    if (header) {
        printf("ICMP Type: %u\n", header->type);
        printf("ICMP Code: %u\n", header->code);
        printf("Checksum: 0x%04x\n", header->checksum);
        if (header->type == 8 || header->type == 0) {
            printf("Identifier: 0x%04x\n", header->rest_of_header.echo.identifier);
            printf("Sequence Number: 0x%04x\n", header->rest_of_header.echo.sequence_number);
        }
        free(header);
    } else {
        printf("Failed to parse ICMP packet.\n");
    }

    return 0;
}