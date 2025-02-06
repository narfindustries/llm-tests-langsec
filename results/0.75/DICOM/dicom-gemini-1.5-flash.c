#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct {
    uint16_t group;
    uint16_t element;
} dicom_tag_t;

typedef struct {
    dicom_tag_t tag;
    char* vr;
    uint32_t length;
    uint8_t* value;
} dicom_attribute_t;

hammer_parser dicom_tag_parser() {
    return hammer_map2(hammer_uint16, hammer_uint16,
                       [](uint16_t group, uint16_t element) {
                           dicom_tag_t* tag = malloc(sizeof(dicom_tag_t));
                           tag->group = group;
                           tag->element = element;
                           return tag;
                       });
}

hammer_parser dicom_attribute_parser() {
    return hammer_seq(dicom_tag_parser,
                      hammer_string,
                      hammer_uint32,
                      hammer_bytes,
                      hammer_map4(
                          [](dicom_tag_t* tag, char* vr, uint32_t length, uint8_t* value) {
                              dicom_attribute_t* attr =
                                  malloc(sizeof(dicom_attribute_t));
                              attr->tag = *tag;
                              attr->vr = vr;
                              attr->length = length;
                              attr->value = value;
                              free(tag);
                              return attr;
                          }));
}

int main(int argc, char** argv) {
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

    uint8_t* buffer = malloc(fileSize);
    fread(buffer, 1, fileSize, file);
    fclose(file);

    hammer_parser file_parser = hammer_many(dicom_attribute_parser());

    hammer_result result = hammer_parse(file_parser, buffer, fileSize);

    if (hammer_is_success(result)) {
        hammer_array attributes = hammer_result_value(result);
        for (size_t i = 0; i < hammer_array_size(attributes); ++i) {
            dicom_attribute_t* attr = hammer_array_get(attributes, i);
            printf("Tag: (%04X,%04X), VR: %s, Length: %u\n",
                   attr->tag.group, attr->tag.element, attr->vr,
                   attr->length);
            free(attr->vr);
            free(attr->value);
            free(attr);
        }
        hammer_array_free(attributes);
    } else {
        fprintf(stderr, "Parsing failed: %s\n", hammer_result_error(result));
    }

    free(buffer);
    hammer_result_free(result);
    return 0;
}
