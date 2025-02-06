#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef struct {
    uint16_t group;
    uint16_t element;
    uint32_t vr_length;
    uint8_t* value;
} DicomElement;

hammer_parser uint16_parser(hammer_parser_state* state) {
    return hammer_uint16(state);
}

hammer_parser uint32_parser(hammer_parser_state* state) {
    return hammer_uint32(state);
}

hammer_parser bytes_parser(size_t len)(hammer_parser_state* state) {
    return hammer_bytes(len)(state);
}

hammer_parser dicom_element_parser(hammer_parser_state* state) {
    return hammer_map(state,
                      hammer_seq(state,
                                 uint16_parser,
                                 uint16_parser,
                                 uint32_parser,
                                 hammer_bind(state, bytes_parser, hammer_uint32(state))),
                      ^(hammer_result r) {
                          DicomElement* elem = malloc(sizeof(DicomElement));
                          if (elem == NULL) return hammer_fail("malloc failed");
                          elem->group = *(uint16_t*)r.value[0];
                          elem->element = *(uint16_t*)r.value[1];
                          elem->vr_length = *(uint32_t*)r.value[2];
                          elem->value = (uint8_t*)r.value[3];
                          return hammer_ok(elem);
                      });
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <dicom_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long fileSize = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* buffer = (uint8_t*)malloc(fileSize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, fileSize, file);
    fclose(file);

    hammer_parser_state state;
    hammer_parser_state_init(&state, buffer, fileSize);

    hammer_result result = hammer_many(&state, dicom_element_parser);

    if (result.success) {
        printf("DICOM file parsed successfully.\n");
        DicomElement** elements = (DicomElement**)result.value;
        for (size_t i = 0; elements[i] != NULL; i++) {
            printf("Element %zu: Group=%04X, Element=%04X, Length=%u\n", i, elements[i]->group, elements[i]->element, elements[i]->vr_length);
            free(elements[i]->value);
            free(elements[i]);
        }
        free(elements);
    } else {
        fprintf(stderr, "DICOM parsing failed at offset %zu: %s\n", state.offset, result.error_message);
    }

    free(buffer);
    return 0;
}
