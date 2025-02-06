#include <hammer/hammer.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define PREAMBLE_SIZE 128
#define PREFIX_SIZE 4

typedef struct {
    uint8_t preamble[PREAMBLE_SIZE];
    uint8_t prefix[PREFIX_SIZE];
} dicom_header_t;

typedef struct {
    uint16_t group;
    uint16_t element;
    uint16_t vr;
    uint16_t length;
    uint8_t value[4096];
} data_element_t;

typedef struct {
    uint16_t group;
    uint16_t element;
    uint16_t value_length;
    uint8_t value[4096];
} item_t;

typedef struct {
    uint16_t group;
    uint16_t element;
    uint16_t length;
    item_t items[1024];
} sequence_t;

typedef struct {
    dicom_header_t header;
    data_element_t elements[2048];
    sequence_t sequences[256];
} dicom_t;

int hammer_string(uint8_t* buffer, uint16_t size, uint8_t* data, uint16_t length) {
    if (length != size) {
        return 0;
    }
    memcpy(data, buffer, size);
    return 1;
}

int hammer_bytes(uint8_t* buffer, uint16_t size, uint8_t* data, uint16_t length) {
    if (length != size) {
        return 0;
    }
    memcpy(data, buffer, size);
    return 1;
}

int hammer_uint16(uint8_t* buffer, uint16_t size, uint16_t* value) {
    if (size < 2) {
        return 0;
    }
    *value = (buffer[1] << 8) | buffer[0];
    return 1;
}

typedef int (*parse_func)(uint8_t*, uint16_t, void*);

int hammer_parse(parse_func parser, uint8_t* buffer, uint16_t size, void* data) {
    return parser(buffer, size, data);
}

int dicom_header_parser(uint8_t* buffer, uint16_t size, dicom_header_t* header) {
    if (size < PREAMBLE_SIZE + PREFIX_SIZE) {
        return 0;
    }
    memcpy(header->preamble, buffer, PREAMBLE_SIZE);
    memcpy(header->prefix, buffer + PREAMBLE_SIZE, PREFIX_SIZE);
    return 1;
}

int data_element_parser(uint8_t* buffer, uint16_t size, data_element_t* element) {
    if (size < 8) {
        return 0;
    }
    if (hammer_uint16(buffer, 2, &element->group) == 0) {
        return 0;
    }
    if (hammer_uint16(buffer + 2, 2, &element->element) == 0) {
        return 0;
    }
    if (hammer_uint16(buffer + 4, 2, &element->vr) == 0) {
        return 0;
    }
    if (hammer_uint16(buffer + 6, 2, &element->length) == 0) {
        return 0;
    }
    return 1;
}

int item_parser(uint8_t* buffer, uint16_t size, item_t* item) {
    if (size < 6) {
        return 0;
    }
    if (hammer_uint16(buffer, 2, &item->group) == 0) {
        return 0;
    }
    if (hammer_uint16(buffer + 2, 2, &item->element) == 0) {
        return 0;
    }
    if (hammer_uint16(buffer + 4, 2, &item->value_length) == 0) {
        return 0;
    }
    return 1;
}

int sequence_parser(uint8_t* buffer, uint16_t size, sequence_t* sequence) {
    if (size < 6) {
        return 0;
    }
    if (hammer_uint16(buffer, 2, &sequence->group) == 0) {
        return 0;
    }
    if (hammer_uint16(buffer + 2, 2, &sequence->element) == 0) {
        return 0;
    }
    if (hammer_uint16(buffer + 4, 2, &sequence->length) == 0) {
        return 0;
    }
    return 1;
}

int dicom_parser(uint8_t* buffer, uint16_t size, dicom_t* dicom) {
    if (size < PREAMBLE_SIZE + PREFIX_SIZE) {
        return 0;
    }
    if (hammer_parse(dicom_header_parser, buffer, PREAMBLE_SIZE + PREFIX_SIZE, &dicom->header) == 0) {
        return 0;
    }
    uint8_t* current = buffer + PREAMBLE_SIZE + PREFIX_SIZE;
    for (uint16_t i = 0; i < 2048; i++) {
        if (hammer_parse(data_element_parser, current, 8, &dicom->elements[i]) == 0) {
            return 0;
        }
        current += 8;
    }
    return 1;
}

int main(int argc, char** argv) {
    if (argc < 2) {
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(size);
    fread(buffer, size, 1, file);
    fclose(file);

    dicom_t* dicom = malloc(sizeof(dicom_t));
    if (hammer_parse(dicom_parser, buffer, size, dicom) == 1) {
        printf("DICOM parsed successfully\n");
    } else {
        printf("DICOM parsing failed\n");
    }

    free(buffer);
    free(dicom);

    return 0;
}