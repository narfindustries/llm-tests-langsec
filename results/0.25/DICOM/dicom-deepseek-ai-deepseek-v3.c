#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

HParser *dicom_parser() {
    HParser *uint16 = h_uint16();
    HParser *uint32 = h_uint32();
    HParser *whitespace = h_whitespace(h_ch(' '));
    HParser *string = h_length_value(h_uint16(), h_many(h_whitespace(h_ch(' '))));

    HParser *preamble = h_sequence(h_many(h_ch(' ')), NULL);
    HParser *prefix = h_sequence(h_many(h_ch(' ')), NULL);
    HParser *header = h_sequence(preamble, prefix, NULL);

    HParser *tag = h_sequence(uint16, uint16, NULL);
    HParser *vr = h_length_value(h_uint16(), h_many(h_whitespace(h_ch(' '))));
    HParser *length = h_choice(uint16, uint32, NULL);
    HParser *value = h_length_value(length, h_many(h_whitespace(h_ch(' '))));
    HParser *data_element = h_sequence(tag, vr, length, value, NULL);

    HParser *dicom_file = h_sequence(header, h_many1(data_element), NULL);

    return dicom_file;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input file>\n", argv[0]);
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

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = dicom_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("Parsing successful!\n");
    } else {
        printf("Parsing failed!\n");
    }

    free(buffer);
    return 0;
}