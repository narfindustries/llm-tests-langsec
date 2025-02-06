#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct {
    uint16_t group;
    uint16_t element;
    uint32_t length;
    uint8_t *value;
} DICOMElement;

HParser *parseGroup() {
    return h_uint16();
}

HParser *parseElement() {
    return h_uint16();
}

HParser *parseLength() {
    return h_uint32();
}

HParser *parseValue(uint32_t length) {
    return h_repeat_n(h_uint8(), length);
}

HParser *parseDICOMElement() {
    return h_sequence(
        h_bind(parseGroup(), 
               (HContinuation)h_bind, parseElement(), 
               (HContinuation)h_bind, parseLength(), 
               (HContinuation)parseValue, NULL),
        NULL);
}

HParser *parseDICOMParser() {
    return h_many(parseDICOMElement());
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <DICOM file>\n", argv[0]);
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

    HParser *parser = parseDICOMParser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        DICOMElement **elements = (DICOMElement**)result->ast;
        for (DICOMElement **element = elements; *element; ++element) {
            printf("Group: %04x, Element: %04x, Length: %u\n", (*element)->group, (*element)->element, (*element)->length);
            // Print value if needed
        }
        h_free_parse_result(result);
    } else {
        fprintf(stderr, "Failed to parse DICOM file\n");
    }

    free(buffer);
    return 0;
}