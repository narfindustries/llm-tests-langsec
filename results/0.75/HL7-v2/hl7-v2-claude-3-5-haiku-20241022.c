#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <string.h>

static const HParsedToken* parse_hl7_message(void* p) {
    return h_make_str(p);
}

static const HParsedToken* parse_hl7_segment(void* p) {
    return h_make_str(p);
}

static const HParsedToken* parse_hl7_field(void* p) {
    return h_make_str(p);
}

static HParser* hl7_field_parser() {
    return h_many1(h_ch_range('0', '9'));
}

static HParser* hl7_segment_parser() {
    HParser* field_parser = hl7_field_parser();
    return h_sequence(
        h_many1(h_ch_range('A', 'Z')),
        h_ch('|'),
        h_sepBy(field_parser, h_ch('|')),
        h_optional(h_ch('\r')),
        NULL
    );
}

static HParser* hl7_message_parser() {
    HParser* segment_parser = hl7_segment_parser();
    return h_many1(segment_parser);
}

int main() {
    HParser* parser = hl7_message_parser();
    return 0;
}