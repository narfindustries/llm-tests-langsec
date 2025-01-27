#include <stdio.h>
#include <stdint.h>
#include <string.h>

// Define the structure for TIFF header
typedef struct {
    uint16_t byte_order;
    uint16_t tiff_magic;
    uint32_t ifd_offset;
} tiff_header_t;

// Define the structure for IFD entry
typedef struct {
    uint16_t tag;
    uint16_t type;
    uint32_t count;
    uint32_t value;
} ifd_entry_t;

// Define the structure for IFD
typedef struct {
    uint16_t num_entries;
    ifd_entry_t entries[10]; // Assuming a maximum of 10 entries
    uint32_t next_ifd_offset;
} ifd_t;

// Function to parse TIFF file
void parse_tiff_file(const char* filename) {
    FILE* file = fopen(filename, "rb");
    if (!file) {
        printf("Error opening file\n");
        return;
    }

    tiff_header_t header;
    fread(&header, sizeof(header), 1, file);

    // Check byte order
    if (header.byte_order == 0x4949) {
        // Little-endian
    } else if (header.byte_order == 0x4d4d) {
        // Big-endian
    } else {
        printf("Invalid byte order\n");
        fclose(file);
        return;
    }

    // Check TIFF magic
    if (header.tiff_magic != 0x002a) {
        printf("Invalid TIFF magic\n");
        fclose(file);
        return;
    }

    // Read IFD
    ifd_t ifd;
    fseek(file, header.ifd_offset, SEEK_SET);
    fread(&ifd, sizeof(ifd), 1, file);

    // Iterate through IFD entries
    for (int i = 0; i < ifd.num_entries; i++) {
        ifd_entry_t entry = ifd.entries[i];
        printf("Tag: %d, Type: %d, Count: %d, Value: %d\n", entry.tag, entry.type, entry.count, entry.value);
    }

    fclose(file);
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        printf("Usage: %s <tiff_file>\n", argv[0]);
        return 1;
    }

    parse_tiff_file(argv[1]);
    return 0;
}