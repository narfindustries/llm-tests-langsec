#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Basic HL7 delimiters
static const uint8_t FIELD_SEP = '|';
static const uint8_t COMPONENT_SEP = '^';
static const uint8_t REPEAT_SEP = '~';
static const uint8_t ESCAPE_CHAR = '\\';
static const uint8_t SUBCOMPONENT_SEP = '&';

// Forward declarations
HParser* build_msh_parser(void);
HParser* build_pid_parser(void);
HParser* build_hl7_message(void);

// MSH segment parser
HParser* build_msh_parser(void) {
    return h_sequence(
        h_ch('M'), h_ch('S'), h_ch('H'),
        h_ch(FIELD_SEP),
        h_ch(COMPONENT_SEP),
        h_ch(REPEAT_SEP),
        h_ch(ESCAPE_CHAR),
        h_ch(SUBCOMPONENT_SEP),
        h_many1(h_not_in((uint8_t*)"\r\n", 2)),
        NULL
    );
}

// PID segment parser
HParser* build_pid_parser(void) {
    return h_sequence(
        h_ch('P'), h_ch('I'), h_ch('D'),
        h_ch(FIELD_SEP),
        h_many1(h_not_in((uint8_t*)"\r\n", 2)),
        NULL
    );
}

// Complete HL7 message parser
HParser* build_hl7_message(void) {
    return h_sequence(
        build_msh_parser(),
        h_many(build_pid_parser()),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open input file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t *input = malloc(size);
    if (!input) {
        fclose(f);
        fprintf(stderr, "Memory allocation failed\n");
        return 1;
    }

    if (fread(input, 1, size, f) != size) {
        free(input);
        fclose(f);
        fprintf(stderr, "Failed to read input file\n");
        return 1;
    }
    fclose(f);

    HParser *parser = build_hl7_message();
    HParseResult *result = h_parse(parser, input, size);

    if (!result) {
        fprintf(stderr, "Parse failed\n");
        free(input);
        return 1;
    }

    // Successfully parsed
    printf("HL7 message parsed successfully\n");

    h_parse_result_free(result);
    free(input);
    return 0;
}