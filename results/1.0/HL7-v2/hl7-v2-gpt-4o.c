#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *create_hl7_v2_parser() {
    // HL7 v2 field separator (|) and components separator (^~\&)
    HParser *field_separator = h_token("|", 1);
    HParser *component_separator = h_token("^~\\&", 4);

    // Character set for simple field
    HParser *simple_char = h_choice(h_ch_range('A', 'Z'), h_ch_range('a', 'z'), h_ch_range('0', '9'), h_ch('-'), h_ch(' '), NULL);
    HParser *simple_field = h_many1(simple_char);

    // MSH Segment Parser (illustrative, starting fields only)
    HParser *msh_segment = h_sequence(
        h_token("MSH", 3),
        field_separator,
        component_separator,
        field_separator,
        simple_field,  // Sending Application
        field_separator,
        simple_field,  // Sending Facility
        field_separator,
        simple_field,  // Receiving Application
        field_separator,
        simple_field,  // Receiving Facility
        field_separator,
        simple_field,  // Date/Time of Message
        field_separator,
        simple_field,  // Security
        field_separator,
        simple_field,  // Message Type
        field_separator,
        simple_field,  // Message Control ID
        field_separator,
        simple_field,  // Processing ID
        field_separator,
        simple_field,  // Version ID
        NULL
    );

    // Full message parser starting with MSH Segment
    HParser *hl7_message = h_many1(msh_segment);

    return hl7_message;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("fopen");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    size_t length = ftell(file);
    fseek(file, 0, SEEK_SET);

    char *data = malloc(length);
    if (!data) {
        perror("malloc");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(data, 1, length, file);
    fclose(file);

    HParser *parser = create_hl7_v2_parser();
    HParseResult *result = h_parse(parser, data, length);

    if (result) {
        printf("Parsing Successful!\n");
        // Process result->ast or result data here
    } else {
        fprintf(stderr, "Parsing Failed!\n");
    }

    h_parse_result_free(result);
    h_free(parser);  // Using h_free as h_parser_free is not a standard Hammer function
    free(data);

    return result ? EXIT_SUCCESS : EXIT_FAILURE;
}