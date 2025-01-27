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
    uint32_t idat_length;
    uint8_t idat_data[1024];
    uint32_t iend_length;
    uint8_t iend_type[4];
    uint32_t iend_crc;
} png_image_t;

// Define the Hammer specification
typedef struct {
    uint8_t magic[4];
    uint32_t version;
    uint32_t image_count;
    png_image_t images[10];
} hammer_t;

// Define the functions to read and write the Hammer specification
hammer_t* read_hammer(const char* filename) {
    FILE* file = fopen(filename, "rb");
    if (!file) {
        return NULL;
    }

    hammer_t* hammer = malloc(sizeof(hammer_t));
    if (!hammer) {
        fclose(file);
        return NULL;
    }

    // Read the magic and version
    if (fread(hammer->magic, 1, 4, file) != 4) {
        free(hammer);
        fclose(file);
        return NULL;
    }
    if (fread(&hammer->version, 1, 4, file) != 4) {
        free(hammer);
        fclose(file);
        return NULL;
    }

    // Read the image count
    if (fread(&hammer->image_count, 1, 4, file) != 4) {
        free(hammer);
        fclose(file);
        return NULL;
    }

    // Read the images
    for (int i = 0; i < hammer->image_count; i++) {
        png_image_t* image = &hammer->images[i];

        // Read the IHDR chunk
        if (fread(&image->ihdr_length, 1, 4, file) != 4) {
            free(hammer);
            fclose(file);
            return NULL;
        }
        if (fread(image->ihdr_type, 1, 4, file) != 4) {
            free(hammer);
            fclose(file);
            return NULL;
        }
        if (fread(&image->width, 1, 4, file) != 4) {
            free(hammer);
            fclose(file);
            return NULL;
        }
        if (fread(&image->height, 1, 4, file) != 4) {
            free(hammer);
            fclose(file);
            return NULL;
        }
        if (fread(&image->bit_depth, 1, 1, file) != 1) {
            free(hammer);
            fclose(file);
            return NULL;
        }
        if (fread(&image->color_type, 1, 1, file) != 1) {
            free(hammer);
            fclose(file);
            return NULL;
        }
        if (fread(&image->compression_method, 1, 1, file) != 1) {
            free(hammer);
            fclose(file);
            return NULL;
        }
        if (fread(&image->filter_method, 1, 1, file) != 1) {
            free(hammer);
            fclose(file);
            return NULL;
        }
        if (fread(&image->interlace_method, 1, 1, file) != 1) {
            free(hammer);
            fclose(file);
            return NULL;
        }
        if (fread(&image->crc, 1, 4, file) != 4) {
            free(hammer);
            fclose(file);
            return NULL;
        }

        // Read the IDAT chunk
        if (fread(&image->idat_length, 1, 4, file) != 4) {
            free(hammer);
            fclose(file);
            return NULL;
        }
        if (fread(image->idat_data, 1, image->idat_length, file) != image->idat_length) {
            free(hammer);
            fclose(file);
            return NULL;
        }

        // Read the IEND chunk
        if (fread(&image->iend_length, 1, 4, file) != 4) {
            free(hammer);
            fclose(file);
            return NULL;
        }
        if (fread(image->iend_type, 1, 4, file) != 4) {
            free(hammer);
            fclose(file);
            return NULL;
        }
        if (fread(&image->iend_crc, 1, 4, file) != 4) {
            free(hammer);
            fclose(file);
            return NULL;
        }
    }

    fclose(file);
    return hammer;
}

int write_hammer(const char* filename, hammer_t* hammer) {
    FILE* file = fopen(filename, "wb");
    if (!file) {
        return -1;
    }

    // Write the magic and version
    if (fwrite(hammer->magic, 1, 4, file) != 4) {
        fclose(file);
        return -1;
    }
    if (fwrite(&hammer->version, 1, 4, file) != 4) {
        fclose(file);
        return -1;
    }

    // Write the image count
    if (fwrite(&hammer->image_count, 1, 4, file) != 4) {
        fclose(file);
        return -1;
    }

    // Write the images
    for (int i = 0; i < hammer->image_count; i++) {
        png_image_t* image = &hammer->images[i];

        // Write the IHDR chunk
        if (fwrite(&image->ihdr_length, 1, 4, file) != 4) {
            fclose(file);
            return -1;
        }
        if (fwrite(image->ihdr_type, 1, 4, file) != 4) {
            fclose(file);
            return -1;
        }
        if (fwrite(&image->width, 1, 4, file) != 4) {
            fclose(file);
            return -1;
        }
        if (fwrite(&image->height, 1, 4, file) != 4) {
            fclose(file);
            return -1;
        }
        if (fwrite(&image->bit_depth, 1, 1, file) != 1) {
            fclose(file);
            return -1;
        }
        if (fwrite(&image->color_type, 1, 1, file) != 1) {
            fclose(file);
            return -1;
        }
        if (fwrite(&image->compression_method, 1, 1, file) != 1) {
            fclose(file);
            return -1;
        }
        if (fwrite(&image->filter_method, 1, 1, file) != 1) {
            fclose(file);
            return -1;
        }
        if (fwrite(&image->interlace_method, 1, 1, file) != 1) {
            fclose(file);
            return -1;
        }
        if (fwrite(&image->crc, 1, 4, file) != 4) {
            fclose(file);
            return -1;
        }

        // Write the IDAT chunk
        if (fwrite(&image->idat_length, 1, 4, file) != 4) {
            fclose(file);
            return -1;
        }
        if (fwrite(image->idat_data, 1, image->idat_length, file) != image->idat_length) {
            fclose(file);
            return -1;
        }

        // Write the IEND chunk
        if (fwrite(&image->iend_length, 1, 4, file) != 4) {
            fclose(file);
            return -1;
        }
        if (fwrite(image->iend_type, 1, 4, file) != 4) {
            fclose(file);
            return -1;
        }
        if (fwrite(&image->iend_crc, 1, 4, file) != 4) {
            fclose(file);
            return -1;
        }
    }

    fclose(file);
    return 0;
}

int main() {
    // Create a sample Hammer specification
    hammer_t hammer;
    memcpy(hammer.magic, "HAMR", 4);
    hammer.version = 1;
    hammer.image_count = 1;

    png_image_t image;
    image.ihdr_length = 25;
    memcpy(image.ihdr_type, "IHDR", 4);
    image.width = 1024;
    image.height = 768;
    image.bit_depth = 8;
    image.color_type = 2;
    image.compression_method = 0;
    image.filter_method = 0;
    image.interlace_method = 0;
    image.crc = 0x12345678;
    image.idat_length = 1024;
    memset(image.idat_data, 0, 1024);
    image.iend_length = 0;
    memcpy(image.iend_type, "IEND", 4);
    image.iend_crc = 0x12345678;

    hammer.images[0] = image;

    // Write the Hammer specification to a file
    write_hammer("output.hammer", &hammer);

    // Read the Hammer specification from the file
    hammer_t* read_hammer_spec = read_hammer("output.hammer");
    if (read_hammer_spec) {
        printf("Read Hammer specification successfully\n");
        free(read_hammer_spec);
    } else {
        printf("Failed to read Hammer specification\n");
    }

    return 0;
}