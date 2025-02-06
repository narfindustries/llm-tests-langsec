#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Define a parser for a DICOM data element
HParser *dicom_data_element() {
    // DICOM data element consists of a tag, VR, length, and value
    HParser *tag = h_sequence(h_uint16(), h_uint16(), NULL); // Group and Element
    HParser *vr = h_choice(
        h_token("AE", 2), h_token("AS", 2), h_token("AT", 2), h_token("CS", 2),
        h_token("DA", 2), h_token("DS", 2), h_token("DT", 2), h_token("FL", 2),
        h_token("FD", 2), h_token("IS", 2), h_token("LO", 2), h_token("LT", 2),
        h_token("OB", 2), h_token("OD", 2), h_token("OF", 2), h_token("OW", 2),
        h_token("PN", 2), h_token("SH", 2), h_token("SL", 2), h_token("SQ", 2),
        h_token("SS", 2), h_token("ST", 2), h_token("TM", 2), h_token("UI", 2),
        h_token("UL", 2), h_token("UN", 2), h_token("UR", 2), h_token("US", 2),
        h_token("UT", 2), NULL);
    HParser *length = h_uint16(); // Length of the value

    // Use h_length_value to create a parser for the value field
    HParser *value = h_length_value(length, h_uint8());

    return h_sequence(tag, vr, length, value, NULL);
}

// Define a parser for a DICOM file
HParser *dicom_file() {
    // DICOM file starts with a 128-byte preamble and "DICM" prefix
    HParser *preamble = h_repeat_n(h_uint8(), 128);
    HParser *prefix = h_token("DICM", 4);
    HParser *data_elements = h_many(dicom_data_element());

    return h_sequence(preamble, prefix, data_elements, NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <dicom_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    const char *filename = argv[1];
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation failed");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = dicom_file();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result->ast) {
        printf("DICOM file parsed successfully.\n");
        // Example: print the number of data elements parsed
        printf("Number of data elements parsed: %zu\n", result->ast->seq->used);
    } else {
        fprintf(stderr, "Failed to parse DICOM file.\n");
    }

    h_parse_result_free(result);
    free(buffer);
    return EXIT_SUCCESS;
}