#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tiffio.h>

// Structure to represent a TIFF image
typedef struct {
    unsigned char *data;
    uint32_t width;
    uint32_t height;
    uint16_t bitsPerSample;
    uint16_t samplesPerPixel;
    uint16_t photometric;
} TIFFImage;


// Function to load a TIFF image using libtiff
TIFFImage* loadTIFF(const char *filename) {
    TIFF *tif = TIFFOpen(filename, "r");
    if (tif == NULL) {
        perror("Error opening TIFF file");
        return NULL;
    }

    TIFFImage *image = (TIFFImage*)malloc(sizeof(TIFFImage));
    if (image == NULL) {
        perror("Memory allocation failed");
        TIFFClose(tif);
        return NULL;
    }

    uint32_t width, height;
    uint16_t bitsPerSample, samplesPerPixel, photometric;

    TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &width);
    TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &height);
    TIFFGetField(tif, TIFFTAG_BITSPERSAMPLE, &bitsPerSample);
    TIFFGetField(tif, TIFFTAG_SAMPLESPERPIXEL, &samplesPerPixel);
    TIFFGetField(tif, TIFFTAG_PHOTOMETRIC, &photometric);


    image->width = width;
    image->height = height;
    image->bitsPerSample = bitsPerSample;
    image->samplesPerPixel = samplesPerPixel;
    image->photometric = photometric;

    size_t dataSize = width * height * samplesPerPixel * (bitsPerSample / 8);
    image->data = (unsigned char*)malloc(dataSize);
    if (image->data == NULL) {
        perror("Memory allocation failed");
        free(image);
        TIFFClose(tif);
        return NULL;
    }

    if (TIFFReadRGBAImage(tif, width, height, image->data, 0) != 1) {
        perror("Error reading TIFF image");
        free(image->data);
        free(image);
        TIFFClose(tif);
        return NULL;
    }


    TIFFClose(tif);
    return image;
}

//Function to process the image (replace with your actual hammer functionality)
void processImage(TIFFImage* image){
    if(image == NULL) return;
    printf("Image loaded successfully. Width: %u, Height: %u\n", image->width, image->height);

    //Example processing: Invert the image (replace with your actual hammer logic)

    for(size_t i = 0; i < image->width * image->height * image->samplesPerPixel; ++i){
        image->data[i] = 255 - image->data[i];
    }
    printf("Image processed.\n");
}


// Function to save a TIFF image (This function is a placeholder.  A real implementation would require more sophisticated handling of TIFF tags.)
void saveTIFF(const char *filename, TIFFImage *image) {
    //This is a placeholder and needs proper TIFF tag handling for robust saving.
    FILE *fp = fopen(filename, "wb");
    if (fp == NULL) {
        perror("Error opening file for writing");
        return;
    }
    fwrite(image->data, 1, image->width * image->height * image->samplesPerPixel * (image->bitsPerSample / 8), fp);
    fclose(fp);
    printf("Image saved (placeholder save).\n");
}


int main(int argc, char *argv[]) {
    if (argc != 3) {
        fprintf(stderr, "Usage: %s <input_tiff> <output_tiff>\n", argv[0]);
        return 1;
    }

    TIFFImage *image = loadTIFF(argv[1]);
    if (image == NULL) {
        return 1;
    }

    processImage(image);
    saveTIFF(argv[2], image);


    free(image->data);
    free(image);
    return 0;
}
