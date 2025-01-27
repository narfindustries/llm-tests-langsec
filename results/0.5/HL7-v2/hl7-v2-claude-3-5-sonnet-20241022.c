#include <hammer/hammer.h>
#include <stdio.h>

static HParser* init_hl7_parser(void) {
    // Basic elements
    HParser* field_sep = h_ch('|');
    HParser* comp_sep = h_ch('^');
    HParser* rep_sep = h_ch('~');
    HParser* esc_char = h_ch('\\');
    HParser* sub_sep = h_ch('&');
    
    // Character sets
    HParser* printable = h_not_in("\r\n\f\v", 4);
    HParser* text_char = h_choice(h_not_in("|^~\\&\r\n\f\v", 9), 
                                 h_sequence(esc_char, h_choice(h_ch('F'), h_ch('S'), h_ch('R'), h_ch('E'), h_ch('T'), NULL), NULL), 
                                 NULL);
    
    // Field content
    HParser* field_content = h_many(text_char);
    
    // Component
    HParser* component = h_many(text_char);
    
    // Subcomponent
    HParser* subcomponent = h_many(text_char);
    
    // Repetition
    HParser* repetition = h_sepBy(component, rep_sep);
    
    // Field with optional components
    HParser* field = h_sepBy(h_sepBy(h_sepBy(subcomponent, sub_sep), comp_sep), rep_sep);
    
    // Segment ID
    HParser* segment_id = h_repeat_n(h_in("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789", 36), 3);
    
    // Segment
    HParser* segment = h_sequence(segment_id,
                                field_sep,
                                h_many(h_sequence(field, field_sep, NULL)),
                                h_token("\r\n", 2),
                                NULL);
    
    // Message
    HParser* message = h_many1(segment);
    
    return message;
}

int main(int argc, char** argv) {
    HParser* parser = init_hl7_parser();
    
    if (!parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }
    
    return 0;
}