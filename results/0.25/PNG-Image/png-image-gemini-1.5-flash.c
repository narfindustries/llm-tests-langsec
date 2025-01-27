#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

// Structure to represent a PNG image header
typedef struct {
    uint8_t signature[8];
    uint32_t width;
    uint32_t height;
    uint8_t bit_depth;
    uint8_t color_type;
    uint8_t compression_method;
    uint8_t filter_method;
    uint8_t interlace_method;
} PNGHeader;


//Simplified PNG reader (only reads header for demonstration)
PNGHeader readPNGHeader(const char* filename) {
    PNGHeader header;
    FILE *fp = fopen(filename, "rb");
    if (!fp) {
        perror("Error opening file");
        exit(1);
    }

    fread(header.signature, 1, 8, fp);
    fread(&header.width, 4, 1, fp);
    fread(&header.height, 4, 1, fp);
    fread(&header.bit_depth, 1, 1, fp);
    fread(&header.color_type, 1, 1, fp);
    fread(&header.compression_method, 1, 1, fp);
    fread(&header.filter_method, 1, 1, fp);
    fread(&header.interlace_method, 1, 1, fp);

    fclose(fp);
    return header;
}


int main() {
    // Example usage:  Replace "test.png" with your actual PNG file.
    //  Error handling for file existence and format should be added for production code.
    PNGHeader header = readPNGHeader("test.png");

    printf("PNG Header:\n");
    printf("Width: %u\n", header.width);
    printf("Height: %u\n", header.height);
    printf("Bit Depth: %u\n", header.bit_depth);
    // Add more header fields as needed

    return 0;
}
