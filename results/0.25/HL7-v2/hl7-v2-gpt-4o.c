#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Define parsers for basic HL7 components
HParser *create_field_separator() { return h_ch('|'); }
HParser *create_component_separator() { return h_ch('^'); }
HParser *create_repetition_separator() { return h_ch('~'); }
HParser *create_escape_character() { return h_ch('\\'); }
HParser *create_subcomponent_separator() { return h_ch('&'); }

// Define a parser for a simple HL7 field (alphanumeric characters)
HParser *create_hl7_field() {
    return h_many1(h_choice(
        h_range('A', 'Z'), 
        h_range('a', 'z'), 
        h_range('0', '9'), 
        h_ch(' '), 
        h_ch('-'), 
        NULL
    ));
}

// Define a parser for an HL7 segment (e.g., MSH)
HParser *create_hl7_segment() {
    return h_sequence(
        create_hl7_field(), create_field_separator(),
        create_hl7_field(), create_field_separator(),
        create_hl7_field(),
        NULL
    );
}

// Define a parser for an HL7 message (one or more segments)
HParser *create_hl7_message() {
    return h_many1(h_sequence(create_hl7_segment(), h_ch('\n'), NULL));
}

void parse_hl7_file(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        return;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    char *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate buffer");
        fclose(file);
        return;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *hl7_message = create_hl7_message();
    HParseResult *result = h_parse(hl7_message, buffer, file_size);
    if (result) {
        printf("Parsing succeeded!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed.\n");
    }

    h_parser_free(hl7_message);
    free(buffer);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <hl7_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    parse_hl7_file(argv[1]);

    return EXIT_SUCCESS;
}