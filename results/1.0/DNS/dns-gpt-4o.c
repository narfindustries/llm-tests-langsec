#include <hammer/hammer.h>

HParsedToken *ip_address(HParseContext *ctx, void *userdata) {
    return h_bits(32, false)->parse(ctx, userdata);
}

HParsedToken *label(HParseContext *ctx, void *userdata) {
    return h_choice(h_bits(8, false), h_many1(h_byte_range('a', 'z')), h_many1(h_byte_range('A', 'Z')))->parse(ctx, userdata);
}

HParsedToken *domain_name(HParseContext *ctx, void *userdata) {
    return h_many_sep(label(ctx, userdata), h_int_value(0x2e, 1))->parse(ctx, userdata);
}

HParsedToken *dns_packet(HParseContext *ctx, void *userdata) {
    return h_sequence(
        h_bits(16, false),     // Transaction ID
        h_bits(16, false),     // Flags
        h_bits(16, false),     // Questions
        h_bits(16, false),     // Answer RRs
        h_bits(16, false),     // Authority RRs
        h_bits(16, false),     // Additional RRs
        h_many1(h_sequence(
            domain_name(ctx, userdata),  // QNAME
            h_bits(16, false),           // QTYPE
            h_bits(16, false)            // QCLASS
        ))
    )->parse(ctx, userdata);
}

int main(int argc, char **argv) {
    HParser *parser = dns_packet(NULL, NULL);
    if (!parser) return 1;

    // Here you would call `h_parse` with the `parser`, input data, and handle errors if any.
    // HParseResult *result = h_parse(parser, <input-data>, <input-length>);
    // if (!result) { fprintf(stderr, "Parse error\n"); return 1; }

    h_delete_parser(parser);
    return 0;
}