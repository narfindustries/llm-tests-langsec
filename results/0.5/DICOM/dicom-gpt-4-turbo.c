#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// DICOM File Format (Simplified)

// Helper function to define a sequence of bytes
static HParser *byte_sequence(const uint8_t *str, size_t len) {
    HParser **parsers = malloc(sizeof(HParser*) * len);
    for (size_t i = 0; i < len; i++) {
        parsers[i] = h_uint8();
    }
    HParser *seq = h_sequence(parsers, len, HParseResult *);
    free(parsers);
    return h_sequence(h_bytes((const uint8_t *)str, len), seq, NULL);
}

// Define a DICOM Tag: group number and element number
static HParser *dicom_tag() {
    return h_sequence(h_uint16(), h_uint16(), NULL);
}

// Define a DICOM Element: tag, VR (Value Representation), length, value
static HParser *dicom_element() {
    return h_sequence(
        dicom_tag(),
        h_bytes(2),  // VR is 2 bytes
        h_uint16(),  // Length of the value field (16-bit unsigned integer)
        h_bytes(1),  // Value field (variable length)
        NULL
    );
}

// Define a DICOM File: Preamble, prefix, and a sequence of elements
static HParser *dicom_file() {
    return h_sequence(
        h_bytes(128),  // Preamble: 128 byte fixed length
        byte_sequence((uint8_t *)"DICM", 4),  // DICOM prefix: "DICM"
        h_many(dicom_element()),  // Sequence of DICOM elements
        NULL
    );
}

int main(int argc, char *argv[]) {
    HParser *dicom_parser = dicom_file();
    FILE *fp = fopen("example.dcm", "rb");
    if (!fp) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    uint8_t *buffer = malloc(size);
    if (!buffer) {
        perror("Failed to allocate buffer");
        fclose(fp);
        return EXIT_FAILURE;
    }

    fread(buffer, 1, size, fp);
    fclose(fp);

    HParseResult *result = h_parse(dicom_parser, buffer, size);
    if (result) {
        printf("DICOM file parsed successfully.\n");
    } else {
        printf("Failed to parse DICOM file.\n");
    }

    free(buffer);
    h_parse_result_free(result);
    h_parser_free(dicom_parser);

    return EXIT_SUCCESS;
}
This C code uses the Hammer parsing library to parse a DICOM file, including its preamble, prefix, and a sequence of elements defined by tags, value representations, lengths, and values. The main function opens a DICOM file, reads it into memory, and attempts to parse it using the defined grammar. If successful, it prints a success message; otherwise, it indicates a failure. This code handles memory allocation and deallocation and ensures the file is properly closed.