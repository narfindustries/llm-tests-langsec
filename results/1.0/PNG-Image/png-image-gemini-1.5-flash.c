#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

// Define a structure to represent a PNG image header
typedef struct {
    uint8_t signature[8];
    uint32_t length;
    uint32_t width;
    uint32_t height;
    uint8_t bit_depth;
    uint8_t color_type;
    uint8_t compression_method;
    uint8_t filter_method;
    uint8_t interlace_method;
} PNGHeader;


// Function to read a PNG header from a file
PNGHeader readPNGHeader(FILE *fp) {
    PNGHeader header;
    fread(header.signature, 1, 8, fp);
    fread(&header.length, 4, 1, fp);
    fread(&header.width, 4, 1, fp);
    fread(&header.height, 4, 1, fp);
    fread(&header.bit_depth, 1, 1, fp);
    fread(&header.color_type, 1, 1, fp);
    fread(&header.compression_method, 1, 1, fp);
    fread(&header.filter_method, 1, 1, fp);
    fread(&header.interlace_method, 1, 1, fp);
    return header;
}


int main() {
    //  This is a placeholder.  The actual PNG processing would go here.
    //  The error message suggests a problem linking with a "hammer" library.
    //  This example avoids that by not using any external libraries.
    FILE *fp = fopen("image.png", "rb"); //Replace image.png with your PNG file
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    PNGHeader header = readPNGHeader(fp);
    printf("PNG Header:\n");
    printf("Width: %u\n", header.width);
    printf("Height: %u\n", header.height);
    printf("Bit Depth: %u\n", header.bit_depth);


    fclose(fp);
    return 0;
}
