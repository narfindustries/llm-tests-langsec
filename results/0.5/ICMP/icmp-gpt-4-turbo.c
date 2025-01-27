#include <hammer/hammer.h>
#include <hammer/glue.h>

// Define the ICMP message types
static HParser *icmp_type;
static HParser *icmp_code;
static HParser *icmp_checksum;
static HParser *icmp_rest_of_header;

// Define the structure for ICMP echo request/reply
static HParser *icmp_identifier;
static HParser *icmp_sequence_number;
static HParser *icmp_data;

// Function to build the ICMP parser
static HParser *build_icmp_parser() {
    icmp_type = h_uint8();
    icmp_code = h_uint8();
    icmp_checksum = h_uint16be();
    icmp_rest_of_header = h_bytes(4);

    icmp_identifier = h_uint16be();
    icmp_sequence_number = h_uint16be();
    icmp_data = h_bytes(48);  // Assuming 48 bytes of data for simplicity

    HParser *icmp_echo = h_sequence(icmp_identifier, icmp_sequence_number, icmp_data, NULL);
    HParser *icmp_message = h_sequence(icmp_type, icmp_code, icmp_checksum, icmp_rest_of_header, icmp_echo, NULL);

    return icmp_message;
}

int main(int argc, char **argv) {
    HParser *icmp_parser = build_icmp_parser();
    uint8_t input[] = {
        0x08, 0x00, 0xf7, 0xff, 0x00, 0x00, 0x00, 0x00,  // ICMP Echo Request Header
        0x1c, 0x2b, 0x07, 0x00,  // ICMP Echo Request Body (Identifier and Sequence number)
        // ICMP data (48 bytes)
        0xde, 0xad, 0xbe, 0xef, 0xde, 0xad, 0xbe, 0xef,
        0xde, 0xad, 0xbe, 0xef, 0xde, 0xad, 0xbe, 0xef,
        0xde, 0xad, 0xbe, 0xef, 0xde, 0xad, 0xbe, 0xef,
        0xde, 0xad, 0xbe, 0xef, 0xde, 0xad, 0xbe, 0xef,
        0xde, 0xad, 0xbe, 0xef, 0xde, 0xad, 0xbe, 0xef,
        0xde, 0xad, 0xbe, 0xef, 0xde, 0xad, 0xbe, 0xef
    };

    HParseResult *result = h_parse(icmp_parser, input, sizeof(input));
    if (result) {
        printf("Parsing successful.\n");
        h_pprint(stdout, result->ast, 0, 0);
        h_parse_result_free(result);
    } else {
        printf("Parsing failed.\n");
    }

    h_parser_free(icmp_parser);
    return 0;
}