#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define the HL7 v2 message structure
HParser *hl7_v2_parser() {
    return h_sequence(
        h_choice(
            h_token("MSH"),
            h_token("FHS"),
            h_token("BHS")
        ),
        h_many1(h_choice(
            h_sequence(
                h_ch('|'),
                h_many1(h_ch_range(32, 126)) // ASCII printable characters
            ),
            h_sequence(
                h_ch('^'),
                h_many1(h_ch_range(32, 126)) // ASCII printable characters
            ),
            h_sequence(
                h_ch('~'),
                h_many1(h_ch_range(32, 126)) // ASCII printable characters
            ),
            h_sequence(
                h_ch('\\'),
                h_many1(h_ch_range(32, 126)) // ASCII printable characters
            ),
            h_sequence(
                h_ch('&'),
                h_many1(h_ch_range(32, 126)) // ASCII printable characters
            )
        )),
        h_end_p()
    );
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <hl7_v2_message>\n", argv[0]);
        return 1;
    }

    const char *input = argv[1];
    HParseResult *result = h_parse(hl7_v2_parser(), (const uint8_t*)input, strlen(input));

    if (result) {
        printf("Parsing succeeded!\n");
        h_parse_result_free(result);
        return 0;
    } else {
        fprintf(stderr, "Parsing failed!\n");
        return 1;
    }
}