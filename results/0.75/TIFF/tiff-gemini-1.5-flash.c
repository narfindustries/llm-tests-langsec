#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

// Structure to represent a TIFF header
typedef struct {
    uint16_t byteOrder;
    uint16_t magicNumber;
} TIFFHeader;


// Function to read a TIFF header from a file
TIFFHeader readTIFFHeader(FILE *file) {
    TIFFHeader header;
    fread(&header.byteOrder, sizeof(uint16_t), 1, file);
    fread(&header.magicNumber, sizeof(uint16_t), 1, file);
    return header;
}


int main() {
    //Simulate TIFF file processing.  Replace with actual TIFF library calls if needed.
    FILE *fp = fopen("test.tiff", "rb"); //Replace with your TIFF file
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    TIFFHeader header = readTIFFHeader(fp);

    if (header.byteOrder != 0x4949 && header.byteOrder != 0x4D4D) {
        fprintf(stderr, "Invalid byte order in TIFF header.\n");
        fclose(fp);
        return 1;
    }

    if (header.magicNumber != 42) {
        fprintf(stderr, "Invalid magic number in TIFF header.\n");
        fclose(fp);
        return 1;
    }


    printf("TIFF Header successfully read!\n");
    fclose(fp);
    return 0;
}
