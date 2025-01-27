#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <unistd.h>

// Define a structure to represent the GZIP file format
typedef struct {
    uint8_t id1;
    uint8_t id2;
    uint8_t cm;
    uint8_t flags;
    uint32_t mtime;
    uint8_t xflags;
    uint8_t os;
} gzip_header_t;

typedef struct {
    uint32_t crc;
    uint32_t isize;
} gzip_footer_t;

// Function to read a GZIP file
bool read_gzip_file(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        return false;
    }

    // Read the GZIP header
    gzip_header_t header;
    if (fread(&header, sizeof(gzip_header_t), 1, file) != 1) {
        fclose(file);
        return false;
    }

    // Check the magic numbers
    if (header.id1 != 0x1f || header.id2 != 0x8b) {
        fclose(file);
        return false;
    }

    // Check the compression method
    if (header.cm != 8) {
        fclose(file);
        return false;
    }

    // Read the compressed data
    uint8_t *data = NULL;
    size_t data_size = 0;
    size_t data_cap = 0;
    bool data_alloc_failed = false;
    while (true) {
        if (data_size >= data_cap) {
            size_t new_cap = data_cap == 0 ? 1024 : data_cap * 2;
            uint8_t *new_data = realloc(data, new_cap);
            if (!new_data) {
                data_alloc_failed = true;
                break;
            }
            data = new_data;
            data_cap = new_cap;
        }

        size_t bytes_read = fread(data + data_size, 1, data_cap - data_size, file);
        if (bytes_read == 0) {
            break;
        }
        data_size += bytes_read;
    }

    if (data_alloc_failed) {
        free(data);
        fclose(file);
        return false;
    }

    // Read the GZIP footer
    gzip_footer_t footer;
    if (fread(&footer, sizeof(gzip_footer_t), 1, file) != 1) {
        free(data);
        fclose(file);
        return false;
    }

    // Close the file
    fclose(file);

    // Free the data
    free(data);

    return true;
}

int main() {
    const char *filename = "output_hammer.gz";
    bool result = read_gzip_file(filename);
    return result ? 0 : 1;
}