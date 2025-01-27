#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Haiku parsing specification
static HParser* haiku_parser() {
    // Define syllable count constraints for haiku structure
    HParser* syllable = h_choice(
        h_token("a"), h_token("an"), h_token("the"),
        h_token("in"), h_token("on"), h_token("at"),
        h_token("my"), h_token("your"), h_token("his"),
        h_token("her"), h_token("its")
    );

    // First line: 5 syllables
    HParser* first_line = h_repeat_n(syllable, 5);

    // Second line: 7 syllables  
    HParser* second_line = h_repeat_n(syllable, 7);

    // Third line: 5 syllables
    HParser* third_line = h_repeat_n(syllable, 5);

    // Complete haiku structure
    HParser* haiku = h_sequence(
        first_line, 
        h_token("\n"),
        second_line,
        h_token("\n"), 
        third_line,
        NULL
    );

    return haiku;
}

int main() {
    HParser* parser = haiku_parser();
    return 0;
}