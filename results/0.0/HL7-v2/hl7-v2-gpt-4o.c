#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Function prototypes
HParser *create_hl7_field_parser();
HParser *create_hl7_segment_parser();
HParser *create_hl7_message_parser();

// Define parsers for HL7 components
HParser *create_hl7_field_parser() {
    return h_many1(h_choice(h_class("AZaz09"), h_ch(' '), NULL));
}

HParser *create_hl7_segment_parser() {
    return h_sequence(
        h_many1(h_choice(h_class("AZaz"), NULL)), // Segment ID
        h_ch('|'), // Field Separator
        h_many(h_sequence(create_hl7_field_parser(), h_ch('|'), NULL)), // Fields
        h_end_p(), // End of segment
        NULL
    );
}

HParser *create_hl7_message_parser() {
    return h_many1(create_hl7_segment_parser());
}

// Function to parse HL7 message from file
void parse_hl7_file(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    char *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *hl7_message = create_hl7_message_parser();
    HParseResult *result = h_parse(hl7_message, buffer, file_size);
    if (result) {
        printf("HL7 message parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse HL7 message.\n");
    }

    free(buffer);
    h_parser_free(hl7_message);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <hl7_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    parse_hl7_file(argv[1]);
    return EXIT_SUCCESS;
}