#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#define HAMMER_NO_MAIN
#define HAMMER_NO_PARSE_ERROR

typedef enum {
    VR_AE,
    VR_AS,
    VR_AT,
    VR_CS,
    VR_DA,
    VR_DS,
    VR_DT,
    VR_FL,
    VR_FD,
    VR_IS,
    VR_LO,
    VR_LT,
    VR_OB,
    VR_OD,
    VR_OF,
    VR_OL,
    VR_OW,
    VR_PN,
    VR_SH,
    VR_SL,
    VR_SQ,
    VR_SS,
    VR_ST,
    VR_TM,
    VR_UI,
    VR_UL,
    VR_US,
    VR_UT
} vr_t;

typedef struct {
    uint16_t group;
    uint16_t element;
    vr_t vr;
    uint32_t length;
    uint8_t* value;
} attribute_t;

typedef struct {
    uint16_t group;
    uint16_t element;
    vr_t vr;
    uint32_t length;
    uint8_t* value;
    attribute_t* items;
    uint32_t count;
} sequence_t;

typedef struct {
    uint16_t group;
    uint16_t element;
    vr_t vr;
    uint32_t length;
    uint8_t* value;
    sequence_t* sequences;
    uint32_t sequence_count;
    attribute_t* attributes;
    uint32_t attribute_count;
} dataset_t;

void dicom_file(void* input, void* output) {
    h_sequence(input, output, 3,
        h_bind("dataset", h_parser(dicom_dataset)),
        h_bind("sequences", h_parser(dicom_sequences)),
        h_bind("attributes", h_parser(dicom_attributes))
    );
}

void dicom_dataset(void* input, void* output) {
    h_sequence(input, output, 5,
        h_bind("group", h_parser(h_uint16)),
        h_bind("element", h_parser(h_uint16)),
        h_bind("vr", h_parser(vr)),
        h_bind("length", h_parser(h_uint32)),
        h_bind("value", h_parser(h_bytes))
    );
}

void dicom_sequences(void* input, void* output) {
    h_zero_or_more(input, output,
        h_bind("sequence", h_parser(dicom_sequence))
    );
}

void dicom_sequence(void* input, void* output) {
    h_sequence(input, output, 5,
        h_bind("group", h_parser(h_uint16)),
        h_bind("element", h_parser(h_uint16)),
        h_bind("vr", h_parser(vr)),
        h_bind("length", h_parser(h_uint32)),
        h_bind("items", h_parser(dicom_items))
    );
}

void dicom_items(void* input, void* output) {
    h_zero_or_more(input, output,
        h_bind("item", h_parser(dicom_item))
    );
}

void dicom_item(void* input, void* output) {
    h_sequence(input, output, 5,
        h_bind("group", h_parser(h_uint16)),
        h_bind("element", h_parser(h_uint16)),
        h_bind("vr", h_parser(vr)),
        h_bind("length", h_parser(h_uint32)),
        h_bind("value", h_parser(h_bytes))
    );
}

void dicom_attributes(void* input, void* output) {
    h_zero_or_more(input, output,
        h_bind("attribute", h_parser(dicom_attribute))
    );
}

void dicom_attribute(void* input, void* output) {
    h_sequence(input, output, 5,
        h_bind("group", h_parser(h_uint16)),
        h_bind("element", h_parser(h_uint16)),
        h_bind("vr", h_parser(vr)),
        h_bind("length", h_parser(h_uint32)),
        h_bind("value", h_parser(h_bytes))
    );
}

void vr(void* input, void* output) {
    h_choice(input, output,
        h_bind("vr", h_lit_string("AE")),
        h_bind("vr", h_lit_string("AS")),
        h_bind("vr", h_lit_string("AT")),
        h_bind("vr", h_lit_string("CS")),
        h_bind("vr", h_lit_string("DA")),
        h_bind("vr", h_lit_string("DS")),
        h_bind("vr", h_lit_string("DT")),
        h_bind("vr", h_lit_string("FL")),
        h_bind("vr", h_lit_string("FD")),
        h_bind("vr", h_lit_string("IS")),
        h_bind("vr", h_lit_string("LO")),
        h_bind("vr", h_lit_string("LT")),
        h_bind("vr", h_lit_string("OB")),
        h_bind("vr", h_lit_string("OD")),
        h_bind("vr", h_lit_string("OF")),
        h_bind("vr", h_lit_string("OL")),
        h_bind("vr", h_lit_string("OW")),
        h_bind("vr", h_lit_string("PN")),
        h_bind("vr", h_lit_string("SH")),
        h_bind("vr", h_lit_string("SL")),
        h_bind("vr", h_lit_string("SQ")),
        h_bind("vr", h_lit_string("SS")),
        h_bind("vr", h_lit_string("ST")),
        h_bind("vr", h_lit_string("TM")),
        h_bind("vr", h_lit_string("UI")),
        h_bind("vr", h_lit_string("UL")),
        h_bind("vr", h_lit_string("US")),
        h_bind("vr", h_lit_string("UT"))
    );
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
    long size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(size);
    fread(buffer, 1, size, file);
    fclose(file);

    void* parser = h_parser_new(dicom_file);
    void* result = h_parse(parser, buffer, size);

    if (result) {
        dataset_t* dataset = (dataset_t*)result;
        printf("Dataset:\n");
        printf("  Group: %u\n", dataset->group);
        printf("  Element: %u\n", dataset->element);
        printf("  VR: %u\n", dataset->vr);
        printf("  Length: %u\n", dataset->length);
        printf("  Value: %s\n", dataset->value);

        for (uint32_t i = 0; i < dataset->sequence_count; i++) {
            sequence_t* sequence = &dataset->sequences[i];
            printf("Sequence %u:\n", i);
            printf("  Group: %u\n", sequence->group);
            printf("  Element: %u\n", sequence->element);
            printf("  VR: %u\n", sequence->vr);
            printf("  Length: %u\n", sequence->length);
            printf("  Items:\n");
            for (uint32_t j = 0; j < sequence->count; j++) {
                attribute_t* item = &sequence->items[j];
                printf("    Item %u:\n", j);
                printf("      Group: %u\n", item->group);
                printf("      Element: %u\n", item->element);
                printf("      VR: %u\n", item->vr);
                printf("      Length: %u\n", item->length);
                printf("      Value: %s\n", item->value);
            }
        }

        for (uint32_t i = 0; i < dataset->attribute_count; i++) {
            attribute_t* attribute = &dataset->attributes[i];
            printf("Attribute %u:\n", i);
            printf("  Group: %u\n", attribute->group);
            printf("  Element: %u\n", attribute->element);
            printf("  VR: %u\n", attribute->vr);
            printf("  Length: %u\n", attribute->length);
            printf("  Value: %s\n", attribute->value);
        }
    } else {
        printf("Error parsing DICOM file\n");
    }

    free(buffer);
    return 0;
}