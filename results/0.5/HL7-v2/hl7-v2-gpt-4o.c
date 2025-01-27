#include <hammer/hammer.h>

HParser *create_hl7_v2_parser() {
    // Define the basic elements
    HParser *digit = h_ch_range('0', '9');
    HParser *dot = h_ch('.');
    HParser *dash = h_ch('-');
    HParser *colon = h_ch(':');
    HParser *space = h_ch(' ');
    HParser *newline = h_ch('\n');
    HParser *carriage_return = h_ch('\r');

    // Define HL7 delimiters
    HParser *field_separator = h_ch('|');
    HParser *component_separator = h_ch('^');
    HParser *repetition_separator = h_ch('~');
    HParser *escape_character = h_ch('\\');
    HParser *subcomponent_separator = h_ch('&');

    // Define alphanumeric and special characters
    HParser *alpha = h_choice(h_ch_range('A', 'Z'), h_ch_range('a', 'z'), NULL);
    HParser *alphanumeric = h_choice(alpha, digit, NULL);
    HParser *special_chars = h_choice(dot, dash, colon, space, NULL);
    HParser *text_data = h_many1(h_choice(alphanumeric, special_chars, NULL));

    // Define HL7 segment terminator
    HParser *segment_terminator = h_choice(newline, carriage_return, NULL);

    // Define a field
    HParser *field = h_many1(h_choice(alphanumeric, special_chars, NULL));

    // Define a segment (e.g., MSH, PID)
    HParser *segment_id = h_repeat_n(alpha, 3);
    HParser *segment = h_sequence(segment_id, field_separator, h_list(field, field_separator), segment_terminator, NULL);

    // Define the HL7 message
    HParser *hl7_message = h_many1(segment);

    return hl7_message;
}

int main() {
    HParser *parser = create_hl7_v2_parser();
    // Example usage of the parser with an HL7 message
    const char *hl7_message = "MSH|^~\\&|SendingApp|SendingFac|ReceivingApp|ReceivingFac|202310111230||ADT^A01|MSG00001|P|2.5\rPID|1||123456^^^Hospital^MR||Doe^John^A||19800101|M|||123 Main St^^Hometown^CA^12345||(555)555-5555|||M||123456789|987-65-4320||||||\r";
    HParseResult *result = h_parse(parser, (const uint8_t *)hl7_message, strlen(hl7_message));

    if (result) {
        printf("HL7 message parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse HL7 message.\n");
    }

    h_parser_free(parser);
    return 0;
}