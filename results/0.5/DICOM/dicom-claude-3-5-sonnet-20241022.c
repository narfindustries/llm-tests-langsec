#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    uint32_t group;
    uint32_t element;
    char vr[3];
    uint32_t length;
    uint8_t *value;
} DicomElement;

static HParser *init_dicom_parser(void) {
    HParser *dicom_group = h_uint32();
    HParser *dicom_element = h_uint32();
    HParser *uint8_parser = h_uint8();
    HParser *two_bytes = h_repeat_n(uint8_parser, 2);
    HParser *dicom_vr = h_length_value(two_bytes, uint8_parser);
    HParser *dicom_length = h_uint32();
    HParser *dicom_value = h_length_value(uint8_parser, h_many(uint8_parser));
    
    HParser *element_parser = h_sequence(dicom_group,
                                       dicom_element,
                                       dicom_vr,
                                       dicom_length,
                                       dicom_value,
                                       NULL);
    
    return h_many1(element_parser);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <dicom_file>\n", argv[0]);
        return 1;
    }

    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t *input = malloc(size);
    if (!input) {
        fclose(f);
        perror("Failed to allocate memory");
        return 1;
    }

    if (fread(input, 1, size, f) != size) {
        free(input);
        fclose(f);
        perror("Failed to read file");
        return 1;
    }

    HParser *parser = init_dicom_parser();
    HParseResult *result = h_parse(parser, input, size);
    
    if (!result) {
        fprintf(stderr, "Failed to parse DICOM file\n");
        free(input);
        fclose(f);
        return 1;
    }

    h_parse_result_free(result);
    free(input);
    fclose(f);
    return 0;
}