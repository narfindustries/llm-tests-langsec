#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define parser for a simple name structure
static HParser* name_parser() {
    return h_choice(
        h_sequence(
            h_many1(h_alpha()),  // First name must have at least one alphabetic character
            h_optional(h_sequence(
                h_ch(' '),       // Optional space
                h_many1(h_alpha()) // Optional last name
            )), 
            NULL
        ),
        NULL
    );
}

// Define parser for age structure 
static HParser* age_parser() {
    return h_sequence(
        h_int_range(0, 120),     // Age between 0-120 
        NULL
    );
}

// Combined person parser 
static HParser* person_parser() {
    return h_sequence(
        h_bom(),                 // Beginning of match
        name_parser(),            // Name parser 
        h_ch(' '),                // Space separator
        age_parser(),             // Age parser
        h_eom(),                  // End of match
        NULL
    );
}

// Main parsing function
int main(int argc, char** argv) {
    // Initialize Hammer 
    h_init();

    // Create person parser
    HParser* parser = person_parser();

    // Sample input 
    const char* input = "John Smith 35"; 

    // Parse input
    HParseResult* result = h_parse(parser, 
        (const uint8_t*)input, 
        strlen(input)
    );

    // Check parsing result
    if (result && result->ast) {
        printf("Successful parse\n");
        h_delete_parse_result(result);
    } else {
        printf("Parsing failed\n");
    }

    // Cleanup 
    h_destroy();
    return 0;
}