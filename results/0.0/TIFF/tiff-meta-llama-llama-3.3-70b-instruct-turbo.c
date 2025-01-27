#include <stdio.h>
#include <stdint.h>
#include <string.h>

// Define the TIFF header structure
typedef struct {
    uint16_t byte_order;
    uint16_t tiff_magic;
    uint32_t ifd_offset;
} tiff_header_t;

// Define the IFD entry structure
typedef struct {
    uint16_t tag;
    uint16_t type;
    uint32_t count;
    uint32_t value;
} ifd_entry_t;

// Define the IFD structure
typedef struct {
    uint16_t num_entries;
    ifd_entry_t entries[10]; // Assuming 10 entries for simplicity
    uint32_t next_ifd_offset;
} ifd_t;

// Define the TIFF structure
typedef struct {
    tiff_header_t header;
    ifd_t ifd;
} tiff_t;

// Function to parse the TIFF file
void parse_tiff(const char* data, size_t size) {
    tiff_t tiff;
    memcpy(&tiff.header, data, sizeof(tiff_header_t));

    // Check the byte order and TIFF magic
    if (tiff.header.byte_order != 0x4949 && tiff.header.byte_order != 0x4d4d) {
        printf("Invalid byte order\n");
        return;
    }
    if (tiff.header.tiff_magic != 0x002a) {
        printf("Invalid TIFF magic\n");
        return;
    }

    // Parse the IFD
    const char* ifd_data = data + tiff.header.ifd_offset;
    memcpy(&tiff.ifd, ifd_data, sizeof(ifd_t));

    // Print the IFD entries
    for (int i = 0; i < tiff.ifd.num_entries; i++) {
        printf("Tag: %u, Type: %u, Count: %u, Value: %u\n",
               tiff.ifd.entries[i].tag,
               tiff.ifd.entries[i].type,
               tiff.ifd.entries[i].count,
               tiff.ifd.entries[i].value);
    }
}

int main() {
    const char data[] = {
        0x49, 0x49, 0x2a, 0x00, 0x08, 0x00, 0x00, 0x00, // TIFF header
        0x10, 0x00, 0x02, 0x00, 0x01, 0x00, 0x00, 0x00, // IFD entry 1
        0x20, 0x00, 0x03, 0x00, 0x02, 0x00, 0x00, 0x00, // IFD entry 2
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00  // Next IFD offset
    };
    parse_tiff(data, sizeof(data));
    return 0;
}