#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define the HL7 v2 message structure
HParser *hl7_v2_parser() {
    H_RULE(segment, h_sequence(
        h_choice(
            h_sequence(
                h_ch('M'),
                h_ch('S'),
                h_ch('H'),
                NULL
            ),
            h_sequence(
                h_ch('P'),
                h_ch('I'),
                h_ch('D'),
                NULL
            ),
            h_sequence(
                h_ch('P'),
                h_ch('V'),
                h_ch('1'),
                NULL
            ),
            NULL
        ),
        h_many1(h_ch('|')),
        NULL
    ));

    H_RULE(message, h_sequence(
        h_many1(segment),
        h_end_p(),
        NULL
    ));

    return message;
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <hl7_v2_message>\n", argv[0]);
        return 1;
    }

    const char *input = argv[1];
    HParser *parser = hl7_v2_parser();
    HParseResult *result = h_parse(parser, (const uint8_t*)input, strlen(input));

    if (result) {
        printf("Parsing succeeded!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }

    h_parser_free(parser);
    return 0;
}