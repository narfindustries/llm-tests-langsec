#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// DICOM Parser using Hammer Parser Combinators
typedef struct {
    uint16_t group;
    uint16_t element;
    uint16_t vr;
    uint32_t length;
    uint8_t* value;
} DicomElement;

// Parse DICOM Preamble (128 bytes + DICM)
static HParser* dicom_preamble() {
    return h_sequence(h_repeat_n(h_uint8(), 128), 
                     h_token((uint8_t*)"DICM", 4), 
                     NULL);
}

// Value Representations
static HParser* explicit_vr() {
    return h_choice(h_token((uint8_t*)"AE", 2),
                   h_token((uint8_t*)"AS", 2),
                   h_token((uint8_t*)"AT", 2),
                   h_token((uint8_t*)"CS", 2),
                   h_token((uint8_t*)"DA", 2),
                   h_token((uint8_t*)"DS", 2),
                   h_token((uint8_t*)"DT", 2),
                   h_token((uint8_t*)"FL", 2),
                   h_token((uint8_t*)"FD", 2),
                   h_token((uint8_t*)"IS", 2),
                   h_token((uint8_t*)"LO", 2),
                   h_token((uint8_t*)"LT", 2),
                   h_token((uint8_t*)"PN", 2),
                   h_token((uint8_t*)"SH", 2),
                   h_token((uint8_t*)"SL", 2),
                   h_token((uint8_t*)"SQ", 2),
                   h_token((uint8_t*)"SS", 2),
                   h_token((uint8_t*)"ST", 2),
                   h_token((uint8_t*)"TM", 2),
                   h_token((uint8_t*)"UI", 2),
                   h_token((uint8_t*)"UL", 2),
                   h_token((uint8_t*)"UN", 2),
                   h_token((uint8_t*)"US", 2),
                   h_token((uint8_t*)"UT", 2),
                   NULL);
}

// DICOM Element parser
static HParser* dicom_element() {
    return h_sequence(
        h_uint16(), // group
        h_uint16(), // element
        explicit_vr(),
        h_uint16(), // length
        h_length_value(h_uint16(), h_uint8()), // value
        NULL
    );
}

// Complete DICOM parser
static HParser* dicom_parser() {
    return h_sequence(
        dicom_preamble(),
        h_many(dicom_element()),
        NULL
    );
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <dicom_file>\n", argv[0]);
        return 1;
    }

    FILE* fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("Failed to open file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t* buffer = malloc(size);
    if (!buffer) {
        fclose(fp);
        fprintf(stderr, "Memory allocation failed\n");
        return 1;
    }

    if (fread(buffer, 1, size, fp) != size) {
        free(buffer);
        fclose(fp);
        fprintf(stderr, "Failed to read file\n");
        return 1;
    }

    HParseResult* result = h_parse(dicom_parser(), buffer, size);
    if (!result) {
        fprintf(stderr, "Failed to parse DICOM file\n");
    } else {
        printf("Successfully parsed DICOM file\n");
        h_parse_result_free(result);
    }

    free(buffer);
    fclose(fp);
    return result ? 0 : 1;
}