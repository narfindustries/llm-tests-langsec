#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// Define the PNG image structure
typedef struct {
    uint8_t signature[8];
    uint32_t ihdr_length;
    uint8_t ihdr_type[4];
    uint32_t width;
    uint32_t height;
    uint8_t bit_depth;
    uint8_t color_type;
    uint8_t compression_method;
    uint8_t filter_method;
    uint8_t interlace_method;
    uint32_t crc;
    uint8_t idat_data[];
} png_image_t;

// Define the Hammer specification
typedef struct {
    png_image_t png_image;
} hammer_t;

// Function to parse the PNG image
int parse_png_image(hammer_t* hammer, uint8_t* data, uint32_t length) {
    // Check the PNG image signature
    if (memcmp(data, "\x89\x50\x4e\x47\x0d\x0a\x1a\x0a", 8) != 0) {
        return -1;
    }

    // Parse the IHDR chunk
    if (length < 24) {
        return -1;
    }
    hammer->png_image.ihdr_length = *(uint32_t*)(data + 8);
    memcpy(hammer->png_image.ihdr_type, data + 12, 4);
    hammer->png_image.width = *(uint32_t*)(data + 16);
    hammer->png_image.height = *(uint32_t*)(data + 20);
    hammer->png_image.bit_depth = *(uint8_t*)(data + 24);
    hammer->png_image.color_type = *(uint8_t*)(data + 25);
    hammer->png_image.compression_method = *(uint8_t*)(data + 26);
    hammer->png_image.filter_method = *(uint8_t*)(data + 27);
    hammer->png_image.interlace_method = *(uint8_t*)(data + 28);

    // Parse the IDAT chunk
    if (length < 33) {
        return -1;
    }
    hammer->png_image.idat_data = data + 33;

    return 0;
}

int main() {
    uint8_t data[] = {
        0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a,
        0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44, 0x52,
        0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01,
        0x08, 0x02, 0x00, 0x00, 0x00,
        0x49, 0x44, 0x41, 0x54, 0x78, 0x9c, 0x63, 0x63,
        0x1c, 0x0b, 0x00, 0x01, 0x00, 0x00, 0x00
    };

    hammer_t hammer;
    uint32_t length = sizeof(data);
    int result = parse_png_image(&hammer, data, length);
    if (result != 0) {
        printf("Error parsing PNG image\n");
        return 1;
    }

    return 0;
}