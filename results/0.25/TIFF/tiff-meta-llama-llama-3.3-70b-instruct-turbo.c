#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    uint16_t offset;
    uint16_t length;
} ifd_entry_t;

typedef struct {
    uint16_t tag;
    uint16_t type;
    uint32_t count;
    uint32_t value;
} ifd_field_t;

typedef struct {
    uint16_t num_entries;
    ifd_entry_t entries[1000];
    ifd_field_t fields[1000];
} ifd_t;

typedef struct {
    uint16_t byte_order;
    uint16_t version;
    uint32_t ifd_offset;
    ifd_t ifd;
} tiff_header_t;

void parse_tiff_file(const char* filename) {
    FILE* file = fopen(filename, "rb");
    if (!file) {
        printf("Error opening file\n");
        return;
    }

    tiff_header_t header;
    fread(&header, sizeof(header), 1, file);

    if (header.byte_order != 0x4949 && header.byte_order != 0x4d4d) {
        printf("Invalid byte order\n");
        fclose(file);
        return;
    }

    if (header.version != 0x002a) {
        printf("Invalid version\n");
        fclose(file);
        return;
    }

    uint32_t ifd_offset = header.ifd_offset;
    fseek(file, ifd_offset, SEEK_SET);

    ifd_t ifd;
    fread(&ifd, sizeof(ifd), 1, file);

    for (int i = 0; i < ifd.num_entries; i++) {
        ifd_entry_t entry = ifd.entries[i];
        ifd_field_t field = ifd.fields[i];

        printf("Tag: %u, Type: %u, Count: %u, Value: %u\n", field.tag, field.type, field.count, field.value);
    }

    fclose(file);
}

int main() {
    parse_tiff_file("input.tiff");
    return 0;
}