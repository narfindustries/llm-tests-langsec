#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define parser for Claude Haiku poem structure
static HParser* claude_haiku_parser() {
    // First line: 5 syllables
    HParser* first_line = h_repeat_n(h_choice(
        h_ch('a'), h_ch('e'), h_ch('i'), h_ch('o'), h_ch('u'),
        h_ch('b'), h_ch('c'), h_ch('d'), h_ch('f'), h_ch('g'), 
        h_ch('h'), h_ch('j'), h_ch('k'), h_ch('l'), h_ch('m'), 
        h_ch('n'), h_ch('p'), h_ch('q'), h_ch('r'), h_ch('s'), 
        h_ch('t'), h_ch('v'), h_ch('w'), h_ch('x'), h_ch('y'), h_ch('z')
    ), 5);

    // Second line: 7 syllables 
    HParser* second_line = h_repeat_n(h_choice(
        h_ch('a'), h_ch('e'), h_ch('i'), h_ch('o'), h_ch('u'),
        h_ch('b'), h_ch('c'), h_ch('d'), h_ch('f'), h_ch('g'), 
        h_ch('h'), h_ch('j'), h_ch('k'), h_ch('l'), h_ch('m'), 
        h_ch('n'), h_ch('p'), h_ch('q'), h_ch('r'), h_ch('s'), 
        h_ch('t'), h_ch('v'), h_ch('w'), h_ch('x'), h_ch('y'), h_ch('z')
    ), 7);

    // Third line: 5 syllables
    HParser* third_line = h_repeat_n(h_choice(
        h_ch('a'), h_ch('e'), h_ch('i'), h_ch('o'), h_ch('u'),
        h_ch('b'), h_ch('c'), h_ch('d'), h_ch('f'), h_ch('g'), 
        h_ch('h'), h_ch('j'), h_ch('k'), h_ch('l'), h_ch('m'), 
        h_ch('n'), h_ch('p'), h_ch('q'), h_ch('r'), h_ch('s'), 
        h_ch('t'), h_ch('v'), h_ch('w'), h_ch('x'), h_ch('y'), h_ch('z')
    ), 5);

    // Combine lines with newline separators
    return h_sequence(first_line, h_ch('\n'), second_line, h_ch('\n'), third_line, NULL);
}

int main() {
    HParser* parser = claude_haiku_parser();
    return 0;
}