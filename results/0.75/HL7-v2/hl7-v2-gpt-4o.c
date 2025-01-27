#include <hammer/hammer.h>

HParser *create_hl7_parser() {
    // Define basic parsers
    HParser *digit = h_ch_range('0', '9');
    HParser *non_delimiter = h_not(h_any_of("|\r\n"));
    HParser *delimiter = h_any_of("|\r\n");
    HParser *sep = h_ignore(h_ch('|'));

    // Define a parser for HL7 fields
    HParser *field = h_many(non_delimiter);

    // Define a parser for HL7 segments
    HParser *segment_name = h_sequence(h_ch_range('A', 'Z'), h_ch_range('A', 'Z'), h_ch_range('A', 'Z'), NULL);
    HParser *segment = h_sequence(
        segment_name,
        sep,
        h_list_sep(field, sep),
        h_ignore(h_ch('\r')),
        NULL
    );

    // Define a parser for HL7 messages
    HParser *message = h_many(segment);

    // Return the final HL7 message parser
    return message;
}

int main() {
    // Create the HL7 parser
    HParser *hl7_parser = create_hl7_parser();

    // Example HL7 message
    const char *hl7_message = "MSH|^~\\&|SendingApp|SendingFac|ReceivingApp|ReceivingFac|202310101200||ADT^A01|123456789|P|2.3\rPID|1||123456^^^Hospital^MR||Doe^John||19800101|M|||123 Main St^^Anytown^OH^44123||(216)123-4567|||M\r";

    // Parse the HL7 message
    HParseResult *result = h_parse(hl7_parser, (const uint8_t *)hl7_message, strlen(hl7_message));

    if (result) {
        printf("HL7 message parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse HL7 message.\n");
    }

    // Free the parser
    h_parser_free(hl7_parser);

    return 0;
}
