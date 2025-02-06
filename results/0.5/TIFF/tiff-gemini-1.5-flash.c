#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    uint16_t tag;
    uint16_t type;
    uint32_t count;
    uint8_t *value;
} TIFFField;

typedef struct {
    uint16_t byteOrder;
    uint16_t version;
    TIFFField *fields;
    size_t numFields;
} TIFFHeader;

static uint8_t* read_tiff_value(uint16_t type, uint32_t count, const uint8_t* buffer, size_t* offset) {
    size_t size = 0;
    switch (type) {
        case 1: size = count; break;
        case 2: size = count; break;
        case 3: size = count * 2; break;
        case 4: size = count * 4; break;
        case 5: size = count * 8; break;
        default:
            fprintf(stderr, "Unsupported TIFF type: %u\n", type);
            return NULL;
    }
    uint8_t* value = (uint8_t*)malloc(size);
    if (value == NULL) return NULL;
    memcpy(value, buffer + *offset, size);
    *offset += size;
    return value;
}

static hm_parser_t* tiff_field_parser() {
    return hm_map(
        hm_seq(
            hm_uint16_t(),
            hm_uint16_t(),
            hm_uint32_t(),
            hm_and_then(hm_bytes(hm_uint32_t()),
                        hm_lambda((hm_value_t v){
                            size_t offset = 0;
                            uint16_t type = ((uint16_t*)v.value)[1];
                            uint32_t count = ((uint32_t*)v.value)[2];
                            uint8_t* buffer = ((uint8_t*)v.value) + 6;
                            uint8_t* val = read_tiff_value(type, count, buffer, &offset);
                            free(v.value);
                            return val ? hm_value(val) : hm_fail();
                        })),
            NULL),
        hm_lambda((hm_value_t v){
            TIFFField* field = (TIFFField*)malloc(sizeof(TIFFField));
            if (field == NULL) return hm_fail();
            field->tag = ((uint16_t*)v.value)[0];
            field->type = ((uint16_t*)v.value)[1];
            field->count = ((uint32_t*)v.value)[2];
            field->value = (uint8_t*)v.value[3];
            free(v.value);
            return hm_value(field);
        })
    );
}

static hm_parser_t* tiff_header_parser() {
    return hm_map(
        hm_seq(
            hm_uint16_t(),
            hm_uint16_t(),
            hm_many(tiff_field_parser()),
            NULL),
        hm_lambda((hm_value_t v){
            TIFFHeader* header = (TIFFHeader*)malloc(sizeof(TIFFHeader));
            if (header == NULL) return hm_fail();
            header->byteOrder = ((uint16_t*)v.value)[0];
            header->version = ((uint16_t*)v.value)[1];
            header->fields = (TIFFField*)v.value[2];
            header->numFields = hm_array_size((hm_array_t*)v.value[2]);
            free(v.value);
            return hm_value(header);
        })
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <tiff_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fileSize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buffer = (uint8_t *)malloc(fileSize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }

    fread(buffer, 1, fileSize, fp);
    fclose(fp);

    hm_parser_t *parser = tiff_header_parser();
    hm_result_t result = hm_parse(parser, buffer, fileSize);

    if (result.success) {
        TIFFHeader *header = (TIFFHeader *)result.value;
        printf("Byte Order: %u\n", header->byteOrder);
        printf("Version: %u\n", header->version);
        printf("Number of fields: %zu\n", header->numFields);
        for (size_t i = 0; i < header->numFields; i++) {
            printf("Field %zu: Tag=%u, Type=%u, Count=%u\n", i, header->fields[i].tag, header->fields[i].type, header->fields[i].count);
            free(header->fields[i].value);
        }
        free(header->fields);
        hm_free_result(&result);
    } else {
        fprintf(stderr, "Parsing failed at offset %zu: %s\n", result.offset, result.error);
    }

    free(buffer);
    hm_free_parser(parser);
    return 0;
}
