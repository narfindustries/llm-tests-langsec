#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Function prototypes for creating parsers
HParser *create_hl7_field();
HParser *create_hl7_field_sep();
HParser *create_hl7_segment_name();
HParser *create_hl7_segment();
HParser *create_hl7_message();

// Define HL7 segments and fields using Hammer parser combinators

HParser *create_hl7_field() {
    return h_choice(
        h_many1(h_ch_range('0', '9')),
        h_many1(h_ch_range('A', 'Z')),
        h_many1(h_ch_range('a', 'z')),
        h_ch('-'),
        h_ch('.'),
        NULL
    );
}

HParser *create_hl7_field_sep() {
    return h_ch('|');
}

HParser *create_hl7_segment_name() {
    return h_sequence(
        h_repeat_n(h_ch_range('A', 'Z'), 3),
        h_end_p(),
        NULL
    );
}

HParser *create_hl7_segment() {
    return h_sequence(
        create_hl7_segment_name(),
        h_many(h_sequence(create_hl7_field_sep(), create_hl7_field(), NULL)),
        h_end_p(),
        NULL
    );
}

HParser *create_hl7_message() {
    return h_many(create_hl7_segment());
}

// Utility function to read file into buffer
char *read_file(const char *filename, size_t *length) {
    FILE *f = fopen(filename, "rb");
    if (!f) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }
    fseek(f, 0, SEEK_END);
    *length = ftell(f);
    fseek(f, 0, SEEK_SET);

    char *buffer = (char *)malloc(*length + 1);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(f);
        exit(EXIT_FAILURE);
    }
    fread(buffer, 1, *length, f);
    fclose(f);
    buffer[*length] = '\0';
    return buffer;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <hl7_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    size_t length;
    char *input = read_file(argv[1], &length);

    HParser *hl7_message = create_hl7_message();
    HParseResult *result = h_parse(hl7_message, (const uint8_t *)input, length);
    if (result) {
        printf("HL7 message parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse HL7 message.\n");
    }

    free(input);
    return EXIT_SUCCESS;
}