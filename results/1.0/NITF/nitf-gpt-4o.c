#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <hammer/hammer.h>

// Create parsers for the specific NITF fields using Hammer

HParser *create_nitf_parser() {
    // Basic element parsers
    HParser *file_type = h_token((const uint8_t *)"NITF", 4);
    HParser *file_version = h_token((const uint8_t *)"02.10", 5);
    HParser *complexity_level = h_int_range(h_uint8(), 1, 99); // Numeric range
    HParser *file_title = h_repeat_n(h_ch_range(0x20, 0x7E), 80); // ASCII
    HParser *file_class = h_choice(h_token((const uint8_t *)"U", 1), 
                                   h_token((const uint8_t *)"C", 1),
                                   h_token((const uint8_t *)"S", 1), 
                                   h_token((const uint8_t *)"TS", 2),
                                   h_token((const uint8_t *)"R", 1), NULL);
    HParser *class_system = h_repeat_n(h_ch_range(0x20, 0x7E), 2);
    HParser *class_code = h_repeat_n(h_ch_range(0x20, 0x7E), 11);
    HParser *release_instructions = h_repeat_n(h_ch_range(0x20, 0x7E), 20);
    HParser *handling = h_repeat_n(h_ch_range(0x20, 0x7E), 2);
    HParser *encryption = h_choice(h_uint8_value(0), h_uint8_value(1), NULL);

    // Sequence of all fields that compose the NITF header
    return h_sequence(file_type, file_version, complexity_level, file_title,
                      file_class, class_system, class_code, release_instructions,
                      handling, encryption, NULL);
}

void parse_nitf_file(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    rewind(file);

    unsigned char *data = malloc(length);
    if (!data) {
        perror("Memory allocation failed");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    if (fread(data, 1, length, file) != length) {
        perror("Failed to read file");
        free(data);
        fclose(file);
        exit(EXIT_FAILURE);
    }
    fclose(file);

    HParser *nitf_parser = create_nitf_parser();
    HParseResult *result = h_parse(nitf_parser, data, length);

    if (result->ast) {
        printf("NITF file parsed successfully!\n");
    } else {
        printf("Failed to parse NITF file.\n");
    }

    h_parse_result_free(result);
    free(data);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <nitf_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    parse_nitf_file(argv[1]);
    return EXIT_SUCCESS;
}