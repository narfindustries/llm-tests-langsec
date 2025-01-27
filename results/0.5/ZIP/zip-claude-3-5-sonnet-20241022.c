#include <hammer/hammer.h>
#include <stdio.h>

static HParser* init_zip_parser() {
    // Basic components
    HParser* digit = h_ch_range('0', '9');
    HParser* letter = h_choice(h_ch_range('a', 'z'), h_ch_range('A', 'Z'), NULL);
    HParser* underscore = h_ch('_');
    HParser* dash = h_ch('-');
    HParser* space = h_ch(' ');
    
    // Basic number sequence
    HParser* number = h_many1(digit);
    
    // Alphanumeric sequence with optional separators
    HParser* alphanum = h_many1(h_choice(letter, digit, underscore, dash, NULL));
    
    // ZIP code formats
    // Standard 5-digit
    HParser* zip5 = h_repeat_n(digit, 5);
    
    // ZIP+4 format (5 digits, dash, 4 digits)
    HParser* zip_plus4 = h_sequence(zip5, dash, h_repeat_n(digit, 4), NULL);
    
    // Optional spaces before and after
    HParser* opt_spaces = h_many(space);
    
    // Final parser combining all formats with optional spaces
    return h_sequence(opt_spaces,
                     h_choice(zip_plus4, zip5, NULL),
                     opt_spaces,
                     NULL);
}

HParser* zip_parser;

void init_parser() {
    zip_parser = init_zip_parser();
}

int parse_zip(const char* input, size_t length) {
    HParseResult* result = h_parse(zip_parser, (const uint8_t*)input, length);
    if (result) {
        h_parse_result_free(result);
        return 1;
    }
    return 0;
}