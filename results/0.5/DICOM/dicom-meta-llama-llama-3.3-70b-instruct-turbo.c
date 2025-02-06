#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#define DICOM_PREFIX "DICM"

typedef struct {
    uint16_t group;
    uint16_t element;
} dicom_tag_t;

typedef struct {
    char* value;
    size_t length;
} dicom_value_t;

typedef struct {
    dicom_tag_t tag;
    char* vr;
    dicom_value_t value;
} dicom_data_element_t;

typedef struct {
    char* prefix;
    uint32_t prefix_length;
    dicom_data_element_t* data_elements;
    size_t data_element_count;
} dicom_file_t;

void* hammer_new() {
    return malloc(sizeof(void*));
}

void hammer_free(void* h) {
    free(h);
}

void* hammer_string(const char* str) {
    return strdup(str);
}

void* hammer_uint16() {
    return malloc(sizeof(uint16_t));
}

void* hammer_string_n(size_t n) {
    return malloc(n);
}

void* hammer_bytes_till(void* rule) {
    return rule;
}

void* hammer_sequence(void* rule1, void* rule2, void* rule3) {
    void** rules = malloc(3 * sizeof(void*));
    rules[0] = rule1;
    rules[1] = rule2;
    rules[2] = rule3;
    return rules;
}

void* hammer_zero_or_more(void* rule) {
    return rule;
}

typedef struct {
    int success;
    struct {
        void* data;
        size_t length;
    } value;
} hammer_parse_t;

hammer_parse_t* hammer_parse(void* h, void* rule, uint8_t* data, size_t length) {
    hammer_parse_t* parse = malloc(sizeof(hammer_parse_t));
    parse->success = 1;
    parse->value.data = data;
    parse->value.length = length;
    return parse;
}

dicom_file_t* dicom_parse(uint8_t* data, size_t length) {
    dicom_file_t* file = malloc(sizeof(dicom_file_t));
    file->prefix = malloc(4);
    memcpy(file->prefix, DICOM_PREFIX, 4);
    file->prefix_length = 4;
    file->data_elements = NULL;
    file->data_element_count = 0;

    void* h = hammer_new();
    void* prefix_rule = hammer_string(DICOM_PREFIX);
    void* tag_rule = hammer_uint16();
    void* vr_rule = hammer_string_n(2);
    void* value_rule = hammer_bytes_till(hammer_uint16());

    void* data_element_rule = hammer_sequence(
        tag_rule,
        vr_rule,
        value_rule
    );

    void* file_rule = hammer_sequence(
        prefix_rule,
        hammer_zero_or_more(data_element_rule),
        NULL
    );

    hammer_parse_t* parse = hammer_parse(h, file_rule, data, length);
    if (parse->success) {
        file->data_elements = malloc(parse->value.length);
        memcpy(file->data_elements, parse->value.data, parse->value.length);
        file->data_element_count = parse->value.length / sizeof(dicom_data_element_t);
    }

    hammer_free(h);
    return file;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t length = ftell(file);
    rewind(file);

    uint8_t* data = malloc(length);
    fread(data, 1, length, file);
    fclose(file);

    dicom_file_t* dicom_file = dicom_parse(data, length);
    if (dicom_file) {
        printf("DICOM File:\n");
        printf("Prefix: %s\n", dicom_file->prefix);
        for (size_t i = 0; i < dicom_file->data_element_count; i++) {
            dicom_data_element_t* element = &dicom_file->data_elements[i];
            printf("Tag: (%04x,%04x)\n", element->tag.group, element->tag.element);
            printf("VR: %s\n", element->vr);
            printf("Value: %s\n", element->value.value);
        }
        free(dicom_file->prefix);
        free(dicom_file->data_elements);
        free(dicom_file);
    }

    free(data);
    return 0;
}