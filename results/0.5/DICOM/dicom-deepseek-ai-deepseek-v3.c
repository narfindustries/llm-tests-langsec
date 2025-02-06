#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define DICOM parser combinators
HParser *dicom_parser() {
    // Define basic parsers
    HParser *tag_parser = h_uint16();
    HParser *vr_parser = h_sequence(h_choice(h_ch('A'), h_ch('E'), h_ch('F'), h_ch('G'), h_ch('H'), h_ch('I'), h_ch('L'), h_ch('M'), h_ch('N'), h_ch('O'), h_ch('P'), h_ch('R'), h_ch('S'), h_ch('T'), h_ch('U'), h_ch('X'), h_ch('Z'), NULL), 2);
    HParser *length_parser = h_uint16();
    HParser *value_parser = h_many(h_uint8());

    // Define DICOM data element parser
    HParser *data_element_parser = h_sequence(tag_parser, vr_parser, length_parser, value_parser, NULL);

    // Define DICOM file parser
    HParser *dicom_file_parser = h_many(data_element_parser);

    return dicom_file_parser;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input file>\n", argv[0]);
        return 1;
    }

    // Open the input file
    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    // Read the file into a buffer
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

    // Parse the DICOM file
    HParser *parser = dicom_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);
    if (!result) {
        fprintf(stderr, "Failed to parse DICOM file\n");
        free(buffer);
        return 1;
    }

    // Clean up
    h_parse_result_free(result);
    free(buffer);

    return 0;
}