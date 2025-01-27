#include <hammer/hammer.h>

HParser *create_hl7_v2_parser() {
    // Define separators
    HParser *field_sep = h_ch(':');
    HParser *component_sep = h_ch('^');
    HParser *repetition_sep = h_ch('~');
    HParser *escape_char = h_ch('\\');
    HParser *subcomponent_sep = h_ch('&');
    HParser *segment_terminator = h_ch('\r');

    // Define basic elements
    HParser *text_data = h_many1(h_choice(h_range('A', 'Z'), h_range('a', 'z'), h_range('0', '9'), h_any_char()));
    HParser *field_data = h_many(h_choice(text_data, escape_char));
    HParser *component_data = h_many(h_choice(field_data, component_sep));
    HParser *subcomponent_data = h_many(h_choice(component_data, subcomponent_sep));

    // Define field
    HParser *field = h_many(h_choice(subcomponent_data, repetition_sep));

    // Define segment
    HParser *segment_id = h_repeat_n(text_data, 3);
    HParser *segment = h_sequence(segment_id, field_sep, h_many(field), segment_terminator, NULL);

    // Define message
    HParser *message = h_many1(segment);

    return message;
}

int main() {
    HParser *hl7_parser = create_hl7_v2_parser();
    // Use the parser with some HL7 message data
    // Example: const char *hl7_message = "MSH|^~\\&|...";
    // HParseResult *result = h_parse(hl7_parser, hl7_message, strlen(hl7_message));

    // Cleanup
    h_parser_free(hl7_parser);

    return 0;
}