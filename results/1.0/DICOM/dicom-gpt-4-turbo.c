#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>
#include <string.h>

typedef struct {
    uint16_t group;
    uint16_t element;
} DicomTag;

typedef struct {
    DicomTag tag;
    char vr[2];
    int length;
    void *value;
} DicomElement;

static HParser *dicom_tag() {
    return h_sequence(h_uint16(), h_uint16(), NULL);
}

static HParser *dicom_vr() {
    return h_choice(
        h_sequence(h_ch('A'), h_ch('E'), NULL),
        h_sequence(h_ch('C'), h_ch('S'), NULL),
        h_sequence(h_ch('D'), h_ch('A'), NULL),
        h_sequence(h_ch('D'), h_ch('S'), NULL),
        h_sequence(h_ch('D'), h_ch('T'), NULL),
        NULL
    );
}

static HParser *dicom_length() {
    return h_uint32();
}

static HParser *dicom_value(HAllocator *mm__, const HParseResult *p__) {
    return h_repeat_n(h_uint8(), p__->container->children[2]->uint);
}

static HParser *dicom_element() {
    HParser *elem = h_sequence(dicom_tag(), dicom_vr(), dicom_length(), h_end_p(), NULL);
    return h_action(elem, (HAction) dicom_value, NULL);
}

int parse_dicom_file(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        fprintf(stderr, "Failed to open file %s\n", filename);
        return -1;
    }

    fseek(file, 0, SEEK_END);
    long fsize = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(fsize);
    if (!buffer) {
        fprintf(stderr, "Failed to allocate memory\n");
        fclose(file);
        return -1;
    }

    fread(buffer, fsize, 1, file);
    fclose(file);

    HParser *dicom_parser = dicom_element();
    HParseResult *result = h_parse(dicom_parser, buffer, fsize);
    if (result) {
        printf("DICOM Element parsed successfully\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse DICOM\n");
    }

    free(buffer);
    h_parser_unref(dicom_parser);
    return 0;
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <dicom_file>\n", argv[0]);
        return 1;
    }

    return parse_dicom_file(argv[1]);
}