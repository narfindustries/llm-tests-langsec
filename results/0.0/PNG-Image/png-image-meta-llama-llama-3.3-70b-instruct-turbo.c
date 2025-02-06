#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define CHUNK_SIZE 1024

typedef struct {
    uint32_t width;
    uint32_t height;
    uint8_t bit_depth;
    uint8_t color_type;
    uint8_t compression_method;
    uint8_t filter_method;
    uint8_t interlace_method;
} ihdr_chunk_t;

typedef struct {
    uint8_t red;
    uint8_t green;
    uint8_t blue;
} plte_entry_t;

typedef struct {
    plte_entry_t entries[256];
    uint8_t num_entries;
} plte_chunk_t;

typedef struct {
    uint8_t* data;
    uint32_t size;
} idat_chunk_t;

typedef struct {
    uint32_t crc;
} iend_chunk_t;

typedef struct {
    char type[5];
    uint8_t* data;
    uint32_t size;
    uint32_t crc;
} chunk_t;

typedef struct {
    uint8_t magic_number[8];
    ihdr_chunk_t ihdr;
    plte_chunk_t plte;
    idat_chunk_t idat;
    iend_chunk_t iend;
    chunk_t* chunks;
    uint32_t num_chunks;
} png_image_t;

void* png_magic_number_parser(void* input) {
    static uint8_t magic_number[8] = {0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A};
    if (memcmp(input, magic_number, 8) == 0) {
        return input + 8;
    } else {
        return NULL;
    }
}

void* ihdr_chunk_parser(void* input) {
    ihdr_chunk_t* ihdr = (ihdr_chunk_t*)input;
    input += sizeof(ihdr_chunk_t);
    return input;
}

void* plte_chunk_parser(void* input) {
    plte_chunk_t* plte = (plte_chunk_t*)input;
    input += sizeof(plte_chunk_t);
    return input;
}

void* idat_chunk_parser(void* input) {
    idat_chunk_t* idat = (idat_chunk_t*)input;
    input += sizeof(idat_chunk_t);
    return input;
}

void* iend_chunk_parser(void* input) {
    iend_chunk_t* iend = (iend_chunk_t*)input;
    input += sizeof(iend_chunk_t);
    return input;
}

void* chunk_parser(void* input) {
    chunk_t* chunk = (chunk_t*)input;
    input += sizeof(chunk_t);
    return input;
}

void* png_image_parser(void* input) {
    png_image_t* image = (png_image_t*)input;
    input = png_magic_number_parser(input);
    if (input == NULL) {
        return NULL;
    }
    input = ihdr_chunk_parser(input);
    if (input == NULL) {
        return NULL;
    }
    input = plte_chunk_parser(input);
    if (input == NULL) {
        return NULL;
    }
    input = idat_chunk_parser(input);
    if (input == NULL) {
        return NULL;
    }
    input = iend_chunk_parser(input);
    if (input == NULL) {
        return NULL;
    }
    return input;
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file: %s\n", argv[1]);
        return 1;
    }

    png_image_t image;
    void* input = &image;
    void* result = png_image_parser(input);

    if (result != NULL) {
        printf("PNG image parsed successfully:\n");
        printf("Width: %u\n", image.ihdr.width);
        printf("Height: %u\n", image.ihdr.height);
        printf("Bit depth: %u\n", image.ihdr.bit_depth);
        printf("Color type: %u\n", image.ihdr.color_type);
        printf("Compression method: %u\n", image.ihdr.compression_method);
        printf("Filter method: %u\n", image.ihdr.filter_method);
        printf("Interlace method: %u\n", image.ihdr.interlace_method);
        if (image.plte.num_entries > 0) {
            printf("PLTE chunk:\n");
            for (uint8_t i = 0; i < image.plte.num_entries; i++) {
                printf("Entry %u: (%u, %u, %u)\n", i, image.plte.entries[i].red, image.plte.entries[i].green, image.plte.entries[i].blue);
            }
        }
        if (image.idat.size > 0) {
            printf("IDAT chunk:\n");
            printf("Size: %u\n", image.idat.size);
        }
        printf("IEND chunk:\n");
        printf("CRC: %u\n", image.iend.crc);
    } else {
        printf("Error parsing PNG image\n");
    }

    fclose(file);
    return 0;
}