#include <hammer/hammer.h>

HParser *create_hl7_v2_parser() {
    // HL7 v2 message types are delimited by the vertical bar '|'
    HParser *field_delimiter = h_ch('|');
  
    // HL7 v2 segments end with a carriage return '\r'
    HParser *segment_terminator = h_ch('\r');
    
    // A field in HL7 can be any printable ASCII character excluding '|', '\r', and '^' for simplicity.
    HParser *field = h_many1(h_choice(h_range(' ', '~'), h_range('!', '&'), h_range('(', '<'), h_range('>', '}')));
    
    // HL7 v2 segments start with a 3-character segment identifier, such as "MSH", followed by a field delimiter
    HParser *segment_id = h_sequence(h_ch_range('A', 'Z'), h_ch_range('A', 'Z'), h_ch_range('A', 'Z'), field_delimiter, NULL);
    
    // A segment consists of a starting segment identifier, a series of fields, and a segment terminator.
    HParser *segment = h_sequence(segment_id, h_many(h_sequence(field, field_delimiter, NULL)), h_optional(field), segment_terminator, NULL);
    
    // An HL7 message is composed of segments.
    HParser *message = h_many1(segment);

    // Finalize the parser, which will parse an HL7 message
    return message;
}

int main() {
    HParser *hl7_parser = create_hl7_v2_parser();

    // Example data (not an actual HL7 message, just for illustration)
    const char *data = "MSH|^~\\&|LAB|XYZ|RTF|Denver Health|20131031154855||ORU^R01|568656|P|2.4\rPID|1||123456||Doe^John||19800101|M|||123 Oak St^^Denver^CO^80204\r";

    HParseResult *result = h_parse(hl7_parser, (const uint8_t *)data, strlen(data));

    if (result) {
        printf("HL7 Message parsed successfully!\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse HL7 Message.\n");
    }

    h_parser_close(hl7_parser);
    return 0;
}
