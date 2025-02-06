#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define GIF87_SIGNATURE 0x474946383761
#define GIF89_SIGNATURE 0x474946383961

typedef struct {
    uint32_t signature;
    uint32_t version;
} gif_header_t;

typedef struct {
    uint16_t width;
    uint16_t height;
    uint8_t packed_fields;
    uint8_t background_color_index;
    uint8_t pixel_aspect_ratio;
} logical_screen_descriptor_t;

typedef struct {
    uint8_t red;
    uint8_t green;
    uint8_t blue;
} color_t;

typedef struct {
    logical_screen_descriptor_t screen_descriptor;
    color_t *global_color_table;
    size_t global_color_table_size;
} gif_data_t;

typedef struct {
    uint8_t image_separator;
    uint16_t left;
    uint16_t top;
    uint16_t width;
    uint16_t height;
    uint8_t packed_fields;
} image_descriptor_t;

typedef struct {
    image_descriptor_t descriptor;
    color_t *local_color_table;
    size_t local_color_table_size;
    uint8_t *image_data;
    size_t image_data_size;
} gif_image_t;

typedef struct {
    gif_header_t header;
    gif_data_t data;
    gif_image_t *images;
    size_t image_count;
} gif_t;

int gif_header_parser(const uint8_t **input, size_t *input_size) {
    if (*input_size < 6) return 0;
    gif_header_t *header = malloc(sizeof(gif_header_t));
    header->signature = (*input)[0] << 24 | (*input)[1] << 16 | (*input)[2] << 8 | (*input)[3];
    header->version = (*input)[4] << 24 | (*input)[5] << 16;
    *input += 6;
    *input_size -= 6;
    return 1;
}

int logical_screen_descriptor_parser(const uint8_t **input, size_t *input_size, logical_screen_descriptor_t *descriptor) {
    if (*input_size < 7) return 0;
    descriptor->width = (*input)[0] << 8 | (*input)[1];
    descriptor->height = (*input)[2] << 8 | (*input)[3];
    descriptor->packed_fields = (*input)[4];
    descriptor->background_color_index = (*input)[5];
    descriptor->pixel_aspect_ratio = (*input)[6];
    *input += 7;
    *input_size -= 7;
    return 1;
}

int color_table_parser(const uint8_t **input, size_t *input_size, color_t **table, size_t *table_size) {
    size_t size = (*input)[0] + 1;
    if (*input_size < size * 3 + 1) return 0;
    *table = malloc(size * sizeof(color_t));
    for (size_t i = 0; i < size; i++) {
        (*table)[i].red = (*input)[i * 3 + 1];
        (*table)[i].green = (*input)[i * 3 + 2];
        (*table)[i].blue = (*input)[i * 3 + 3];
    }
    *table_size = size;
    *input += size * 3 + 1;
    *input_size -= size * 3 + 1;
    return 1;
}

int image_descriptor_parser(const uint8_t **input, size_t *input_size, image_descriptor_t *descriptor) {
    if (*input_size < 9) return 0;
    descriptor->image_separator = (*input)[0];
    descriptor->left = (*input)[1] << 8 | (*input)[2];
    descriptor->top = (*input)[3] << 8 | (*input)[4];
    descriptor->width = (*input)[5] << 8 | (*input)[6];
    descriptor->height = (*input)[7] << 8 | (*input)[8];
    descriptor->packed_fields = (*input)[9];
    *input += 10;
    *input_size -= 10;
    return 1;
}

int lzw_compressed_data_parser(const uint8_t **input, size_t *input_size, uint8_t **data, size_t *data_size) {
    size_t size = 0;
    while (*input_size > 0 && (*input)[0] != 0x3b) {
        size++;
        *input += 1;
        *input_size -= 1;
    }
    *data = malloc(size);
    memcpy(*data, *input - size, size);
    *data_size = size;
    return 1;
}

int gif_image_parser(const uint8_t **input, size_t *input_size, gif_image_t *image) {
    if (!image_descriptor_parser(input, input_size, &image->descriptor)) return 0;
    if (!color_table_parser(input, input_size, &image->local_color_table, &image->local_color_table_size)) {
        image->local_color_table = NULL;
        image->local_color_table_size = 0;
    }
    if (!lzw_compressed_data_parser(input, input_size, &image->image_data, &image->image_data_size)) return 0;
    return 1;
}

int gif_parser(const uint8_t **input, size_t *input_size, gif_t *gif) {
    if (!gif_header_parser(input, input_size)) return 0;
    gif->header.signature = GIF87_SIGNATURE;
    gif->header.version = 0x61;
    if (!logical_screen_descriptor_parser(input, input_size, &gif->data.screen_descriptor)) return 0;
    if (!color_table_parser(input, input_size, &gif->data.global_color_table, &gif->data.global_color_table_size)) {
        gif->data.global_color_table = NULL;
        gif->data.global_color_table_size = 0;
    }
    size_t image_count = 0;
    while (*input_size > 0 && (*input)[0] == 0x2c) {
        gif_image_t image;
        if (!gif_image_parser(input, input_size, &image)) break;
        gif->images = realloc(gif->images, (image_count + 1) * sizeof(gif_image_t));
        gif->images[image_count] = image;
        image_count++;
    }
    gif->image_count = image_count;
    if (*input_size > 0 && (*input)[0] != 0x3b) return 0;
    return 1;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file: %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t file_size = ftell(file);
    rewind(file);

    uint8_t *data = malloc(file_size);
    if (!data) {
        printf("Error allocating memory\n");
        return 1;
    }

    size_t read_size = fread(data, 1, file_size, file);
    if (read_size != file_size) {
        printf("Error reading file\n");
        return 1;
    }

    fclose(file);

    gif_t gif;
    if (!gif_parser(&data, &file_size, &gif)) {
        printf("Error parsing GIF\n");
        return 1;
    }

    printf("GIF Signature: 0x%x\n", gif.header.signature);
    printf("GIF Version: 0x%x\n", gif.header.version);
    printf("Logical Screen Width: %u\n", gif.data.screen_descriptor.width);
    printf("Logical Screen Height: %u\n", gif.data.screen_descriptor.height);
    printf("Global Color Table Size: %zu\n", gif.data.global_color_table_size);
    printf("Image Count: %zu\n", gif.image_count);
    for (size_t i = 0; i < gif.image_count; i++) {
        gif_image_t *image = &gif.images[i];
        printf("Image %zu:\n", i);
        printf("  Left: %u\n", image->descriptor.left);
        printf("  Top: %u\n", image->descriptor.top);
        printf("  Width: %u\n", image->descriptor.width);
        printf("  Height: %u\n", image->descriptor.height);
        printf("  Local Color Table Size: %zu\n", image->local_color_table_size);
        printf("  Image Data Size: %zu\n", image->image_data_size);
    }

    free(data);
    return 0;
}