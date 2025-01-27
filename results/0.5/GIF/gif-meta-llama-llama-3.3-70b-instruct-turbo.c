#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// Define the structure for the GIF file format
typedef struct {
    uint8_t signature[3];
    uint8_t version[3];
    uint16_t width;
    uint16_t height;
    uint8_t flags;
    uint8_t background_color_index;
    uint8_t aspect_ratio;
} GifHeader;

typedef struct {
    uint8_t block_size;
    uint8_t* block_data;
} GifBlock;

typedef struct {
    GifHeader header;
    GifBlock* blocks;
    uint32_t block_count;
} GifFile;

// Function to parse the GIF file
int parse_gif(const uint8_t* data, uint32_t size, GifFile* gif) {
    // Check the signature and version
    if (data[0] != 'G' || data[1] != 'I' || data[2] != 'F') {
        return 0;
    }
    if (data[3] != '8' || data[4] != '9' || data[5] != 'a') {
        return 0;
    }

    // Parse the header
    gif->header.width = (data[6] << 8) | data[7];
    gif->header.height = (data[8] << 8) | data[9];
    gif->header.flags = data[10];
    gif->header.background_color_index = data[11];
    gif->header.aspect_ratio = data[12];

    // Parse the blocks
    uint32_t block_index = 13;
    gif->block_count = 0;
    while (block_index < size) {
        // Check the block size
        uint8_t block_size = data[block_index];
        block_index++;

        // Allocate memory for the block
        gif->blocks = realloc(gif->blocks, (gif->block_count + 1) * sizeof(GifBlock));
        gif->blocks[gif->block_count].block_size = block_size;
        gif->blocks[gif->block_count].block_data = malloc(block_size);

        // Copy the block data
        memcpy(gif->blocks[gif->block_count].block_data, &data[block_index], block_size);
        block_index += block_size;

        // Increment the block count
        gif->block_count++;
    }

    return 1;
}

// Function to generate the GIF file
int generate_gif(const GifFile* gif, uint8_t** data, uint32_t* size) {
    // Calculate the total size
    uint32_t total_size = 13; // Header size
    for (uint32_t i = 0; i < gif->block_count; i++) {
        total_size += 1 + gif->blocks[i].block_size; // Block size byte + block data
    }

    // Allocate memory for the GIF data
    *data = malloc(total_size);
    *size = total_size;

    // Copy the header
    (*data)[0] = 'G';
    (*data)[1] = 'I';
    (*data)[2] = 'F';
    (*data)[3] = '8';
    (*data)[4] = '9';
    (*data)[5] = 'a';
    (*data)[6] = (gif->header.width >> 8) & 0xFF;
    (*data)[7] = gif->header.width & 0xFF;
    (*data)[8] = (gif->header.height >> 8) & 0xFF;
    (*data)[9] = gif->header.height & 0xFF;
    (*data)[10] = gif->header.flags;
    (*data)[11] = gif->header.background_color_index;
    (*data)[12] = gif->header.aspect_ratio;

    // Copy the blocks
    uint32_t data_index = 13;
    for (uint32_t i = 0; i < gif->block_count; i++) {
        // Copy the block size
        (*data)[data_index] = gif->blocks[i].block_size;
        data_index++;

        // Copy the block data
        memcpy(&(*data)[data_index], gif->blocks[i].block_data, gif->blocks[i].block_size);
        data_index += gif->blocks[i].block_size;
    }

    return 1;
}

int main() {
    // Create a sample GIF file
    GifFile gif;
    gif.header.width = 100;
    gif.header.height = 100;
    gif.header.flags = 0;
    gif.header.background_color_index = 0;
    gif.header.aspect_ratio = 0;
    gif.block_count = 1;
    gif.blocks = malloc(sizeof(GifBlock));
    gif.blocks[0].block_size = 10;
    gif.blocks[0].block_data = malloc(10);
    for (int i = 0; i < 10; i++) {
        gif.blocks[0].block_data[i] = i;
    }

    // Generate the GIF data
    uint8_t* data;
    uint32_t size;
    generate_gif(&gif, &data, &size);

    // Parse the GIF data
    GifFile parsed_gif;
    parse_gif(data, size, &parsed_gif);

    // Print the parsed GIF file
    printf("Width: %d\n", parsed_gif.header.width);
    printf("Height: %d\n", parsed_gif.header.height);
    printf("Flags: %d\n", parsed_gif.header.flags);
    printf("Background Color Index: %d\n", parsed_gif.header.background_color_index);
    printf("Aspect Ratio: %d\n", parsed_gif.header.aspect_ratio);
    for (uint32_t i = 0; i < parsed_gif.block_count; i++) {
        printf("Block %d Size: %d\n", i, parsed_gif.blocks[i].block_size);
        for (uint32_t j = 0; j < parsed_gif.blocks[i].block_size; j++) {
            printf("%d ", parsed_gif.blocks[i].block_data[j]);
        }
        printf("\n");
    }

    // Free the allocated memory
    free(data);
    for (uint32_t i = 0; i < gif.block_count; i++) {
        free(gif.blocks[i].block_data);
    }
    free(gif.blocks);
    for (uint32_t i = 0; i < parsed_gif.block_count; i++) {
        free(parsed_gif.blocks[i].block_data);
    }
    free(parsed_gif.blocks);

    return 0;
}