#include <hammer/hammer.h>
#include <stdio.h>

static HParser* init_icmp_parser() {
    // Basic numeric parsers
    HParser *uint8 = h_uint8();
    HParser *uint16 = h_uint16();
    HParser *uint32 = h_uint32();

    // Type and code fields
    HParser *type = uint8;
    HParser *code = uint8;
    HParser *checksum = uint16;

    // Rest of header fields
    HParser *identifier = uint16;
    HParser *sequence = uint16;
    HParser *timestamp = uint32;

    // Optional data field
    HParser *data = h_many(uint8);

    // Combine fields in sequence
    return h_sequence(type, code, checksum, identifier, sequence, timestamp, data, NULL);
}

int main() {
    HParser *parser = init_icmp_parser();
    
    if (!parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }

    HParseResult *result = h_parse(parser, NULL, 0);
    
    if (result) {
        h_parse_result_free(result);
    }
    
    return 0;
}