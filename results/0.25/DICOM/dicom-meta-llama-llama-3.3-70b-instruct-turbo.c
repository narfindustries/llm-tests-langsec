#include <hammer/hammer.h>
#include <stdlib.h>
#include <stdio.h>

#define HAMMER_NO_MAIN
#define HAMMER_NO_PARSE_ERROR

typedef enum {
    DICOM_TRANSFER_SYNTAX_EXPLICIT_LITTLE_ENDIAN,
    DICOM_TRANSFER_SYNTAX_EXPLICIT_BIG_ENDIAN,
    DICOM_TRANSFER_SYNTAX_IMPLICIT_LITTLE_ENDIAN,
} dicom_transfer_syntax_t;

typedef struct {
    uint16_t group;
    uint16_t element;
} dicom_tag_t;

typedef struct {
    dicom_tag_t tag;
    uint32_t vr;
    uint32_t length;
    uint8_t* value;
} dicom_data_element_t;

typedef struct {
    dicom_tag_t tag;
    uint32_t length;
    uint8_t* value;
} dicom_item_t;

typedef struct {
    dicom_tag_t tag;
    uint32_t length;
    dicom_item_t* items;
} dicom_sequence_t;

typedef struct {
    dicom_tag_t tag;
    uint32_t length;
    dicom_data_element_t* data_elements;
} dicom_dataset_t;

typedef struct {
    dicom_transfer_syntax_t transfer_syntax;
    dicom_dataset_t* dataset;
} dicom_file_t;

void* hammer_parser_dicom_file(void* input, size_t* size) {
    dicom_file_t* dicom_file = malloc(sizeof(dicom_file_t));
    dicom_file->transfer_syntax = *(dicom_transfer_syntax_t*)input;
    input += sizeof(dicom_transfer_syntax_t);
    *size -= sizeof(dicom_transfer_syntax_t);

    dicom_file->dataset = malloc(sizeof(dicom_dataset_t));
    dicom_file->dataset->tag.group = 0x0002;
    dicom_file->dataset->tag.element = 0x0000;
    dicom_file->dataset->length = 0;
    dicom_file->dataset->data_elements = NULL;

    while (*size > 0) {
        dicom_data_element_t* data_element = malloc(sizeof(dicom_data_element_t));
        data_element->tag.group = *(uint16_t*)input;
        input += sizeof(uint16_t);
        *size -= sizeof(uint16_t);
        data_element->tag.element = *(uint16_t*)input;
        input += sizeof(uint16_t);
        *size -= sizeof(uint16_t);
        data_element->vr = *(uint32_t*)input;
        input += sizeof(uint32_t);
        *size -= sizeof(uint32_t);
        data_element->length = *(uint32_t*)input;
        input += sizeof(uint32_t);
        *size -= sizeof(uint32_t);
        data_element->value = malloc(data_element->length);
        memcpy(data_element->value, input, data_element->length);
        input += data_element->length;
        *size -= data_element->length;

        dicom_file->dataset->length++;
        dicom_file->dataset->data_elements = realloc(dicom_file->dataset->data_elements, dicom_file->dataset->length * sizeof(dicom_data_element_t));
        dicom_file->dataset->data_elements[dicom_file->dataset->length - 1] = *data_element;
        free(data_element);
    }

    return dicom_file;
}

void* hammer_parser_dicom_data_element(void* input, size_t* size) {
    dicom_data_element_t* data_element = malloc(sizeof(dicom_data_element_t));
    data_element->tag.group = *(uint16_t*)input;
    input += sizeof(uint16_t);
    *size -= sizeof(uint16_t);
    data_element->tag.element = *(uint16_t*)input;
    input += sizeof(uint16_t);
    *size -= sizeof(uint16_t);
    data_element->vr = *(uint32_t*)input;
    input += sizeof(uint32_t);
    *size -= sizeof(uint32_t);
    data_element->length = *(uint32_t*)input;
    input += sizeof(uint32_t);
    *size -= sizeof(uint32_t);
    data_element->value = malloc(data_element->length);
    memcpy(data_element->value, input, data_element->length);
    input += data_element->length;
    *size -= data_element->length;

    return data_element;
}

void* hammer_parser_dicom_item(void* input, size_t* size) {
    dicom_item_t* item = malloc(sizeof(dicom_item_t));
    item->tag.group = *(uint16_t*)input;
    input += sizeof(uint16_t);
    *size -= sizeof(uint16_t);
    item->tag.element = *(uint16_t*)input;
    input += sizeof(uint16_t);
    *size -= sizeof(uint16_t);
    item->length = *(uint32_t*)input;
    input += sizeof(uint32_t);
    *size -= sizeof(uint32_t);
    item->value = malloc(item->length);
    memcpy(item->value, input, item->length);
    input += item->length;
    *size -= item->length;

    return item;
}

void* hammer_parser_dicom_sequence(void* input, size_t* size) {
    dicom_sequence_t* sequence = malloc(sizeof(dicom_sequence_t));
    sequence->tag.group = *(uint16_t*)input;
    input += sizeof(uint16_t);
    *size -= sizeof(uint16_t);
    sequence->tag.element = *(uint16_t*)input;
    input += sizeof(uint16_t);
    *size -= sizeof(uint16_t);
    sequence->length = *(uint32_t*)input;
    input += sizeof(uint32_t);
    *size -= sizeof(uint32_t);
    sequence->items = NULL;

    while (*size > 0) {
        dicom_item_t* item = hammer_parser_dicom_item(input, size);
        sequence->length++;
        sequence->items = realloc(sequence->items, sequence->length * sizeof(dicom_item_t));
        sequence->items[sequence->length - 1] = *item;
        free(item);
    }

    return sequence;
}

void* hammer_parser_dicom_dataset(void* input, size_t* size) {
    dicom_dataset_t* dataset = malloc(sizeof(dicom_dataset_t));
    dataset->tag.group = 0x0002;
    dataset->tag.element = 0x0000;
    dataset->length = 0;
    dataset->data_elements = NULL;

    while (*size > 0) {
        dicom_data_element_t* data_element = hammer_parser_dicom_data_element(input, size);
        dataset->length++;
        dataset->data_elements = realloc(dataset->data_elements, dataset->length * sizeof(dicom_data_element_t));
        dataset->data_elements[dataset->length - 1] = *data_element;
        free(data_element);
    }

    return dataset;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <dicom_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file: %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        printf("Error allocating memory\n");
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    size_t size = file_size;
    dicom_file_t* dicom_file = hammer_parser_dicom_file(buffer, &size);
    if (!dicom_file) {
        printf("Error parsing DICOM file\n");
        return 1;
    }

    printf("DICOM File:\n");
    printf("  Transfer Syntax: %d\n", dicom_file->transfer_syntax);
    printf("  Dataset:\n");
    for (int i = 0; i < dicom_file->dataset->length; i++) {
        dicom_data_element_t* data_element = &dicom_file->dataset->data_elements[i];
        printf("    Tag: %04X%04X\n", data_element->tag.group, data_element->tag.element);
        printf("    VR: %04X\n", data_element->vr);
        printf("    Length: %d\n", data_element->length);
        printf("    Value: %s\n", data_element->value);
    }

    free(buffer);
    free(dicom_file->dataset->data_elements);
    free(dicom_file->dataset);
    free(dicom_file);

    return 0;
}