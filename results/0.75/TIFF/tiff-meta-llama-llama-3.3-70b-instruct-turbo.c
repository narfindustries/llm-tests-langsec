#include <stdio.h>
#include <stdint.h>
#include <string.h>

typedef struct {
    uint16_t byte_order;
    uint16_t tiff_version;
    uint32_t ifd_offset;
} tiff_header_t;

typedef struct {
    uint16_t tag;
    uint16_t type;
    uint32_t count;
    uint32_t value;
} ifd_entry_t;

typedef struct {
    tiff_header_t header;
    ifd_entry_t ifd_entries[10];
} tiff_file_t;

void parse_tiff(const char* filename) {
    FILE* file = fopen(filename, "rb");
    if (!file) {
        printf("Failed to open file\n");
        return;
    }

    tiff_file_t tiff_file;
    fread(&tiff_file.header, sizeof(tiff_header_t), 1, file);

    if (tiff_file.header.byte_order != 0x4949 && tiff_file.header.byte_order != 0x4d4d) {
        printf("Invalid byte order\n");
        return;
    }

    if (tiff_file.header.tiff_version != 0x002a) {
        printf("Invalid TIFF version\n");
        return;
    }

    fseek(file, tiff_file.header.ifd_offset, SEEK_SET);
    fread(tiff_file.ifd_entries, sizeof(ifd_entry_t), 10, file);

    for (int i = 0; i < 10; i++) {
        printf("Tag: %u, Type: %u, Count: %u, Value: %u\n", tiff_file.ifd_entries[i].tag, tiff_file.ifd_entries[i].type, tiff_file.ifd_entries[i].count, tiff_file.ifd_entries[i].value);
    }

    fclose(file);
}

int main() {
    parse_tiff("input.tiff");
    return 0;
}