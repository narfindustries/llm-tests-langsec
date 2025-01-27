#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define parser for haiku structure
static HParser* haiku_parser() {
    // Syllable count constraints
    HParser* syllable = h_choice(
        h_token("a", 1), h_token("an", 2), 
        h_token("the", 3), h_token("and", 3),
        h_token("soft", 1), h_token("light", 1)
    );

    // First line: 5 syllables
    HParser* first_line = h_sequence(
        syllable, syllable, syllable, syllable, syllable, 
        NULL
    );

    // Second line: 7 syllables  
    HParser* second_line = h_sequence(
        syllable, syllable, syllable, syllable, 
        syllable, syllable, syllable, 
        NULL
    );

    // Third line: 5 syllables
    HParser* third_line = h_sequence(
        syllable, syllable, syllable, syllable, syllable, 
        NULL
    );

    // Complete haiku structure
    HParser* haiku = h_sequence(
        first_line, h_token("\n", 1), 
        second_line, h_token("\n", 1), 
        third_line, 
        NULL
    );

    return haiku;
}

int main() {
    // Initialize Hammer parser
    HParser* parser = haiku_parser();

    // Example haiku input
    const char* input = "soft light whispers\n"
                        "dancing through autumn shadows\n"
                        "gentle breeze sighs low";

    // Parse input
    HParseResult* result = h_parse(parser, (uint8_t*)input, strlen(input));

    if (result && result->ast) {
        printf("Valid Haiku Detected\n");
        return 0;
    } else {
        printf("Invalid Haiku Structure\n");
        return 1;
    }
}