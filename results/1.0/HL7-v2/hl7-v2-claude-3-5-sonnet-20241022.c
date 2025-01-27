#include <hammer/hammer.h>
#include <stdio.h>

static const char segment_terminator = '\r';
static const char field_separator = '|';
static const char component_separator = '^';
static const char subcomponent_separator = '&';
static const char repetition_separator = '~';

static HParser* build_hl7_parser(void) {
    // Basic character parsers
    HParser* any_char = h_ch_range(32, 126);
    HParser* field_sep = h_ch(field_separator);
    HParser* segment_term = h_ch(segment_terminator);
    
    // Data content parsers
    HParser* data_char = h_choice(h_ch_range(32, 123), h_ch_range(125, 126), NULL);
    HParser* data_content = h_many1(data_char);
    
    // Field content
    HParser* field_content = h_many(h_difference(any_char, h_ch(field_separator)));
    
    // Field
    HParser* field = h_sequence(field_content, field_sep, NULL);
    
    // Segment Header (e.g., MSH, PID)
    HParser* segment_header = h_token("MSH", 3);
    
    // MSH segment specific
    HParser* msh_field = h_sequence(
        h_ch(field_separator),
        h_ch(component_separator),
        h_ch(repetition_separator),
        h_ch(subcomponent_separator),
        NULL
    );
    
    // MSH segment
    HParser* msh_segment = h_sequence(
        segment_header,
        msh_field,
        h_many1(field),
        segment_term,
        NULL
    );
    
    // Generic segment
    HParser* generic_segment_header = h_repeat_n(h_ch_range('A', 'Z'), 3);
    HParser* generic_segment = h_sequence(
        generic_segment_header,
        field_sep,
        h_many1(field),
        segment_term,
        NULL
    );
    
    // Complete message
    HParser* message = h_sequence(
        msh_segment,
        h_many(generic_segment),
        NULL
    );
    
    return message;
}

int main(int argc, char** argv) {
    HParser* parser = build_hl7_parser();
    
    if (!parser) {
        fprintf(stderr, "Failed to create parser\n");
        return 1;
    }
    
    const char* input = "MSH|^~\\&|SENDING_APP|SENDING_FAC|REC_APP|REC_FAC|20240101101530||ADT^A01|MSG00001|P|2.5\r"
                       "PID|||12345^^^MRN||DOE^JOHN^Q||19800101|M\r"
                       "PV1||O|OP^PAREG^^^^^OP|U|||123^DOCTOR^BOB\r";
    
    HParseResult* result = h_parse(parser, (const uint8_t*)input, strlen(input));
    
    if (result) {
        printf("Parsing successful\n");
        h_parse_result_free(result);
        return 0;
    } else {
        fprintf(stderr, "Parsing failed\n");
        return 1;
    }
}