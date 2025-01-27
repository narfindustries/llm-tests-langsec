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
} png_header_t;


//Simplified PNG reader (error handling omitted for brevity)
png_header_t read_png_header(const char* filename) {
    png_header_t header;
    FILE *fp = fopen(filename, "rb");
    fread(header.signature, 1, 8, fp);
    fseek(fp, 4, SEEK_CUR); //skip IHDR length
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
    // Example usage:  Replace "input.png" with your actual PNG file
    png_header_t header = read_png_header("input.png");

    printf("PNG Header Information:\n");
    printf("Width: %u\n", header.width);
    printf("Height: %u\n", header.height);
    printf("Bit Depth: %u\n", header.bit_depth);
    printf("Color Type: %u\n", header.color_type);


    //Further processing or Hammer integration would go here.  
    //The error message suggests a linking issue with the Hammer library.
    //Ensure that the Hammer library is correctly installed and linked during compilation.

    return 0;
}
