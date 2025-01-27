#include <hammer/hammer.h>
#include <hammer/glue.h>

// Define the ICMP message types
enum {
    ICMP_ECHO_REPLY = 0,
    ICMP_DEST_UNREACH = 3,
    ICMP_SOURCE_QUENCH = 4,
    ICMP_REDIRECT = 5,
    ICMP_ECHO_REQUEST = 8,
    ICMP_TIME_EXCEEDED = 11,
    ICMP_PARAMETER_PROBLEM = 12,
    ICMP_TIMESTAMP_REQUEST = 13,
    ICMP_TIMESTAMP_REPLY = 14,
    ICMP_INFO_REQUEST = 15,
    ICMP_INFO_REPLY = 16,
    ICMP_ADDRESS_MASK_REQUEST = 17,
    ICMP_ADDRESS_MASK_REPLY = 18
};

// Define the structure of an ICMP header
static HParser *icmp_header;
static HParser *icmp_message;

static void init_icmp_parser(void) {
    H_RULE(uint8, h_uint8());
    H_RULE(uint16, h_uint16());

    icmp_header = h_sequence(
        uint8,                      // type
        uint8,                      // code
        uint16,                     // checksum
        h_uint32(),                 // rest of the header
        NULL
    );

    icmp_message = h_sequence(
        icmp_header,
        h_bytes(1),                 // payload (at least 1 byte)
        NULL
    );
}

int main(int argc, char **argv) {
    HParser *parser;
    HParseResult *result;
    HBitWriter *bit_writer;
    uint8_t *input;
    size_t input_size;
    size_t bit_length;

    init_icmp_parser();

    // Read input data from stdin or file
    // This part is left as an exercise to the reader
    // input = ...
    // input_size = ...

    parser = icmp_message;
    result = h_parse(parser, input, input_size);

    if (result) {
        printf("Parse successful!\n");
        bit_writer = h_bit_writer_new();
        h_write_result_as_bits(result, bit_writer);
        bit_length = h_bit_writer_bits(bit_writer);
        printf("Bit length: %zu\n", bit_length);
        h_bit_writer_free(bit_writer);
    } else {
        printf("Parse failed!\n");
    }

    h_parse_result_free(result);
    return 0;
}