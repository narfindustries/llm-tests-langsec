#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

// Structure to represent a TIFF header
typedef struct {
    uint16_t byteOrder;
    uint16_t magicNumber;
    uint32_t offset;
} TIFFHeader;


//Simplified TIFF image structure (only relevant fields for this example)
typedef struct {
    uint32_t width;
    uint32_t height;
    uint16_t bitsPerSample;
    uint16_t samplesPerPixel;
    uint8_t* data;
} TIFFImage;


// Function to read a TIFF header
TIFFHeader readTIFFHeader(FILE *file) {
    TIFFHeader header;
    fread(&header, sizeof(TIFFHeader), 1, file);
    return header;
}


// Function to read a simplified TIFF image (assumes minimal metadata)
TIFFImage readTIFFImage(FILE *file, TIFFHeader header) {
    TIFFImage image;
    fseek(file, header.offset, SEEK_SET); //Seek to image data

    //Simplified reading -  replace with actual TIFF parsing if needed.
    fread(&image.width, sizeof(uint32_t), 1, file);
    fread(&image.height, sizeof(uint32_t), 1, file);
    fread(&image.bitsPerSample, sizeof(uint16_t), 1, file);
    fread(&image.samplesPerPixel, sizeof(uint16_t), 1, file);

    size_t dataSize = image.width * image.height * image.bitsPerSample * image.samplesPerPixel / 8;
    image.data = (uint8_t*)malloc(dataSize);
    fread(image.data, dataSize, 1, file);

    return image;
}

int main() {
    FILE *file = fopen("input.tif", "rb");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    TIFFHeader header = readTIFFHeader(file);
    TIFFImage image = readTIFFImage(file, header);


    printf("Image Width: %u\n", image.width);
    printf("Image Height: %u\n", image.height);
    printf("Bits per sample: %u\n", image.bitsPerSample);
    printf("Samples per pixel: %u\n", image.samplesPerPixel);

    //Process image data here (example: print first 10 bytes)

    for (size_t i = 0; i < 10 && i < image.width * image.height * image.bitsPerSample * image.samplesPerPixel /8; ++i) {
        printf("%02X ", image.data[i]);
    }
    printf("\n");

    free(image.data);
    fclose(file);
    return 0;
}
