#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Structure to represent a PNG image header
typedef struct {
    unsigned char signature[8];
    unsigned int width;
    unsigned int height;
    unsigned char bit_depth;
    unsigned char color_type;
    unsigned char compression_method;
    unsigned char filter_method;
    unsigned char interlace_method;
} PNGHeader;


//Simplified PNG writing (no error handling for brevity)
void writePNG(const char* filename, PNGHeader header, unsigned char* imageData) {
    FILE *fp = fopen(filename, "wb");
    fwrite(header.signature, 1, 8, fp);
    //Write other header chunks (simplified for this example)
    fwrite(&header.width, sizeof(unsigned int), 1, fp);
    fwrite(&header.height, sizeof(unsigned int), 1, fp);
    fwrite(&header.bit_depth, 1, 1, fp);
    fwrite(&header.color_type, 1, 1, fp);
    fwrite(&header.compression_method, 1, 1, fp);
    fwrite(&header.filter_method, 1, 1, fp);
    fwrite(&header.interlace_method, 1, 1, fp);

    //Write image data (simplified for this example)
    fwrite(imageData, 1, header.width * header.height * 4, fp); //Assuming RGBA

    fclose(fp);
}


int main() {
    // Example usage:
    PNGHeader header;
    memcpy(header.signature, "\x89PNG\r\n\x1a\n", 8);
    header.width = 100;
    header.height = 100;
    header.bit_depth = 8;
    header.color_type = 6; //RGBA
    header.compression_method = 0;
    header.filter_method = 0;
    header.interlace_method = 0;

    //Allocate memory for image data (replace with your actual image data)
    unsigned char* imageData = (unsigned char*)malloc(header.width * header.height * 4);
    if (imageData == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        return 1;
    }
    //Fill imageData with sample data (replace with your actual image processing)
    memset(imageData, 255, header.width * header.height * 4); //White image


    writePNG("output.png", header, imageData);

    free(imageData);
    return 0;
}
