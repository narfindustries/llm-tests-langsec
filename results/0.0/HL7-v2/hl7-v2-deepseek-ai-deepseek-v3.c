#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define parsers for basic HL7 data types
HParser *hl7_field_separator() {
    return h_ch('|');
}

HParser *hl7_component_separator() {
    return h_ch('^');
}

HParser *hl7_subcomponent_separator() {
    return h_ch('&');
}

HParser *hl7_escape_character() {
    return h_ch('\\');
}

HParser *hl7_repetition_separator() {
    return h_ch('~');
}

HParser *hl7_field() {
    return h_many1(h_choice(hl7_field_separator(), hl7_component_separator(), hl7_subcomponent_separator(), hl7_escape_character(), hl7_repetition_separator(), h_alpha(), h_digit(), h_whitespace(h_alpha()), h_punct(), NULL));
}

HParser *hl7_segment() {
    return h_sequence(h_many1(h_alpha()), hl7_field_separator(), h_many1(hl7_field()), NULL);
}

HParser *hl7_message() {
    return h_many1(hl7_segment());
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = (uint8_t *)malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(hl7_message(), buffer, file_size);
    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }

    free(buffer);
    return 0;
}