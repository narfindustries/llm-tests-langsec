#include <hammer/hammer.h>
#include <stdio.h>

static HParser* init_hl7_parser(void) {
    // Basic components
    HParser* segment_terminator = h_ch('\r');
    HParser* field_separator = h_ch('|');
    HParser* component_separator = h_ch('^');
    HParser* subcomponent_separator = h_ch('&');
    HParser* repetition_separator = h_ch('~');
    HParser* escape_char = h_ch('\\');
    
    // Text content
    HParser* normal_char = h_not_in("\r|^&~\\");
    HParser* escaped_char = h_sequence(escape_char, h_any_char(), NULL);
    HParser* text_char = h_choice(normal_char, escaped_char, NULL);
    HParser* text = h_many(text_char);

    // Field content
    HParser* field_content = text;
    HParser* field = field_content;
    HParser* fields = h_sepBy(field, field_separator);

    // Segment ID
    HParser* segment_id = h_token((const uint8_t*)"MSH", 3);

    // MSH segment
    HParser* msh_segment = h_sequence(segment_id, field_separator, fields, segment_terminator, NULL);

    // Other segments
    HParser* other_segment_id = h_many_m(3, 3, h_alpha());
    HParser* other_segment = h_sequence(other_segment_id, field_separator, fields, segment_terminator, NULL);

    // Complete message
    HParser* segment = h_choice(msh_segment, other_segment, NULL);
    HParser* message = h_many1(segment);

    return message;
}

int main(int argc, char* argv[]) {
    HParser* parser = init_hl7_parser();
    
    if (!parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }

    return 0;
}