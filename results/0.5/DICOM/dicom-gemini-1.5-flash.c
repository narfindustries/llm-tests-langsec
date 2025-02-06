#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    uint16_t tag;
    char vr[3];
    size_t length;
    void *value;
} DicomElement;

HammerParser tagParser() {
    return map(uint16_parser(), (uint16_t tag) -> {
        DicomElement *element = malloc(sizeof(DicomElement));
        element->tag = tag;
        return element;
    });
}

HammerParser vrParser() {
    return map(string_parser(2), (char *vr) -> vr);
}

HammerParser lengthParser() {
    return map(uint32_parser(), (uint32_t len) -> (size_t)len);
}

HammerParser valueParser(size_t len, char *vr) {
    if (strcmp(vr, "OB") == 0) {
        return bytes_parser(len);
    } else if (strcmp(vr, "UI") == 0 || strcmp(vr, "SH") == 0 || strcmp(vr, "CS") == 0 || strcmp(vr, "DA") == 0 || strcmp(vr, "DS") == 0 || strcmp(vr, "IS") == 0 || strcmp(vr, "LO") == 0 || strcmp(vr, "PN") == 0 || strcmp(vr, "ST") == 0 || strcmp(vr, "TM") == 0 || strcmp(vr, "LT") == 0 || strcmp(vr, "UT") == 0 || strcmp(vr, "AE") == 0) {
        return string_parser(len);
    }
    else {
        return string_parser(len);
    }
}

HammerParser dicomElementParser() {
    return flat_map(
        sequence_parser(tagParser(), vrParser(), lengthParser()),
        (DicomElement *element, char *vr, size_t len) ->
            flat_map(valueParser(len, vr), (void *value) -> {
                element->vr = vr;
                element->length = len;
                element->value = value;
                return succeed(element);
            })
    );
}

HammerParser dicomFileParser() {
    return many(dicomElementParser());
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <dicom_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long fileSize = ftell(file);
    fseek(file, 0, SEEK_SET);

    char *buffer = malloc(fileSize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(file);
        return 1;
    }
    fread(buffer, 1, fileSize, file);
    fclose(file);

    HammerResult result = parse(dicomFileParser(), buffer, fileSize);

    if (result.success) {
        List elements = result.value;
        for (size_t i = 0; i < elements.count; i++) {
            DicomElement *element = elements.data[i];
            printf("Tag: 0x%04X, VR: %s, Length: %zu\n", element->tag, element->vr, element->length);
            free(element->value);
            free(element);
        }
        free(elements.data);
    } else {
        fprintf(stderr, "Parsing failed at offset %zu: %s\n", result.offset, result.error);
    }

    free(buffer);
    return 0;
}
