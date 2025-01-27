#include <hammer/hammer.h>
#include <stdio.h>

static HParser* init_hl7_parser(void) {
    // Basic components
    HParser *field_sep = h_ch('|');
    HParser *comp_sep = h_ch('^');
    HParser *rep_sep = h_ch('~');
    HParser *escape_char = h_ch('\\');
    HParser *sub_comp_sep = h_ch('&');
    
    // Text components
    HParser *normal_char = h_not_in("|^~\\&\r\n", 6);
    HParser *escaped_char = h_sequence(escape_char, h_choice(field_sep, comp_sep, rep_sep, escape_char, sub_comp_sep, NULL), NULL);
    HParser *text_char = h_choice(normal_char, escaped_char, NULL);
    HParser *text = h_many(text_char);

    // Field components
    HParser *subcomponent = text;
    HParser *component_content = h_sepBy(subcomponent, sub_comp_sep);
    HParser *component = component_content;
    HParser *field_content = h_sepBy(component, comp_sep);
    HParser *field = field_content;
    HParser *segment_fields = h_sepBy(field, field_sep);

    // Segment components
    HParser *segment_id = h_token((const uint8_t*)"MSH", 3);
    HParser *segment_content = h_sequence(segment_id, field_sep, segment_fields, NULL);
    HParser *segment = h_sequence(segment_content, h_token((const uint8_t*)"\r\n", 2), NULL);

    // Message components
    HParser *message = h_many1(segment);

    return message;
}

int main(int argc, char** argv) {
    HParser *parser = init_hl7_parser();
    
    if (parser == NULL) {
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }

    const char* input = "MSH|^~\\&|SENDING_APP|SENDING_FAC|REC_APP|REC_FAC|20230915||ADT^A01|MSG00001|P|2.3\r\n";
    size_t input_len = strlen(input);

    HParseResult *result = h_parse(parser, (const uint8_t*)input, input_len);
    
    if (!result) {
        fprintf(stderr, "Parse failed\n");
        return 1;
    }

    h_parse_result_free(result);
    return 0;
}