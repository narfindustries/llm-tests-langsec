#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Function prototypes for creating parsers
HParser *create_field_separator();
HParser *create_field();
HParser *create_segment_name();
HParser *create_segment();
HParser *create_hl7_message();

// Function to read the binary file
unsigned char *read_file(const char *filename, size_t *length) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }

    fseek(file, 0, SEEK_END);
    *length = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *data = malloc(*length);
    if (!data) {
        perror("Failed to allocate memory");
        exit(EXIT_FAILURE);
    }

    fread(data, 1, *length, file);
    fclose(file);

    return data;
}

// Main function
int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    size_t length;
    unsigned char *data = read_file(argv[1], &length);

    HParser *hl7_message = create_hl7_message();
    HParseResult *result = h_parse(hl7_message, data, length);
    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Parsing failed.\n");
    }

    h_parser_free(hl7_message);
    free(data);
    return EXIT_SUCCESS;
}

// Function implementations
HParser *create_field_separator() {
    return h_ch('\r');
}

HParser *create_field() {
    return h_many1(h_choice(h_ch_range('A', 'Z'), h_ch_range('0', '9'), h_ch('-'), h_ch('.'), h_ch('&'), h_ch(' ')));
}

HParser *create_segment_name() {
    HParser *field = create_field();
    return h_repeat_n(field, 3);
}

HParser *create_segment() {
    HParser *segment_name = create_segment_name();
    HParser *field = create_field();
    HParser *field_separator = create_field_separator();
    return h_sequence(segment_name, h_many(field), field_separator, NULL);
}

HParser *create_hl7_message() {
    HParser *segment = create_segment();
    return h_many(segment);
}