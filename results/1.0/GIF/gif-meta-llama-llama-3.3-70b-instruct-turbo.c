#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int global_color_table_flag;
    int color_resolution;
    int sort_flag;
    int size_of_global_color_table;
} gif_packed_fields_t;

typedef struct {
    int local_color_table_flag;
    int interlace_flag;
    int sort_flag;
    int reserved;
    int size_of_local_color_table;
} gif_image_packed_fields_t;

typedef struct {
    int size;
    unsigned char *data;
} gif_color_table_t;

typedef struct {
    unsigned char separator;
    int left_position;
    int top_position;
    int width;
    int height;
    gif_image_packed_fields_t packed_fields;
    gif_color_table_t *local_color_table;
} gif_image_descriptor_t;

typedef struct {
    unsigned char signature[3];
    unsigned char version[3];
    int logical_screen_width;
    int logical_screen_height;
    gif_packed_fields_t packed_fields;
    int background_color_index;
    int pixel_aspect_ratio;
    gif_color_table_t *global_color_table;
    gif_image_descriptor_t *image_descriptors;
} gif_t;

int gif_parse_packed_fields(unsigned char *data, int size, gif_packed_fields_t *packed_fields) {
    if (size < 1) return 0;
    packed_fields->global_color_table_flag = (data[0] >> 7) & 1;
    packed_fields->color_resolution = (data[0] >> 4) & 7;
    packed_fields->sort_flag = (data[0] >> 3) & 1;
    packed_fields->size_of_global_color_table = data[0] & 7;
    return 1;
}

int gif_parse_image_packed_fields(unsigned char *data, int size, gif_image_packed_fields_t *packed_fields) {
    if (size < 1) return 0;
    packed_fields->local_color_table_flag = (data[0] >> 7) & 1;
    packed_fields->interlace_flag = (data[0] >> 6) & 1;
    packed_fields->sort_flag = (data[0] >> 5) & 1;
    packed_fields->reserved = (data[0] >> 4) & 1;
    packed_fields->size_of_local_color_table = data[0] & 7;
    return 1;
}

int gif_parse_color_table(unsigned char *data, int size, gif_color_table_t *color_table) {
    int i;
    if (size < 3) return 0;
    color_table->size = 1 << (data[0] & 7);
    if (size < color_table->size * 3) return 0;
    color_table->data = malloc(color_table->size * 3);
    for (i = 0; i < color_table->size; i++) {
        color_table->data[i * 3] = data[1 + i * 3];
        color_table->data[i * 3 + 1] = data[1 + i * 3 + 1];
        color_table->data[i * 3 + 2] = data[1 + i * 3 + 2];
    }
    return 1;
}

int gif_parse_image_descriptor(unsigned char *data, int size, gif_image_descriptor_t *image_descriptor) {
    if (size < 9) return 0;
    image_descriptor->separator = data[0];
    image_descriptor->left_position = (data[1] << 8) | data[2];
    image_descriptor->top_position = (data[3] << 8) | data[4];
    image_descriptor->width = (data[5] << 8) | data[6];
    image_descriptor->height = (data[7] << 8) | data[8];
    if (!gif_parse_image_packed_fields(data + 9, size - 9, &image_descriptor->packed_fields)) return 0;
    if (image_descriptor->packed_fields.local_color_table_flag) {
        gif_color_table_t color_table;
        if (!gif_parse_color_table(data + 10, size - 10, &color_table)) return 0;
        image_descriptor->local_color_table = &color_table;
    } else {
        image_descriptor->local_color_table = NULL;
    }
    return 1;
}

int gif_parse_gif(unsigned char *data, int size, gif_t *gif) {
    int i;
    if (size < 10) return 0;
    gif->signature[0] = data[0];
    gif->signature[1] = data[1];
    gif->signature[2] = data[2];
    if (gif->signature[0] != 'G' || gif->signature[1] != 'I' || gif->signature[2] != 'F') return 0;
    gif->version[0] = data[3];
    gif->version[1] = data[4];
    gif->version[2] = data[5];
    if (gif->version[0] != '8' || gif->version[1] != '9' || gif->version[2] != 'a') return 0;
    gif->logical_screen_width = (data[6] << 8) | data[7];
    gif->logical_screen_height = (data[8] << 8) | data[9];
    if (!gif_parse_packed_fields(data + 10, size - 10, &gif->packed_fields)) return 0;
    gif->background_color_index = data[11];
    gif->pixel_aspect_ratio = data[12];
    if (gif->packed_fields.global_color_table_flag) {
        gif_color_table_t color_table;
        if (!gif_parse_color_table(data + 13, size - 13, &color_table)) return 0;
        gif->global_color_table = &color_table;
    } else {
        gif->global_color_table = NULL;
    }
    gif->image_descriptors = NULL;
    int offset = 13 + (gif->packed_fields.global_color_table_flag ? 3 * (1 << gif->packed_fields.size_of_global_color_table) : 0);
    while (offset < size) {
        gif_image_descriptor_t *image_descriptor = malloc(sizeof(gif_image_descriptor_t));
        if (!gif_parse_image_descriptor(data + offset, size - offset, image_descriptor)) {
            free(image_descriptor);
            return 0;
        }
        if (gif->image_descriptors == NULL) {
            gif->image_descriptors = image_descriptor;
        } else {
            gif_image_descriptor_t *current = gif->image_descriptors;
            while (current->local_color_table != NULL) {
                current = current->local_color_table;
            }
            current->local_color_table = image_descriptor;
        }
        offset += 9 + (image_descriptor->packed_fields.local_color_table_flag ? 3 * (1 << image_descriptor->packed_fields.size_of_local_color_table) : 0);
    }
    return 1;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gif_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        fprintf(stderr, "Failed to open file '%s'\n", argv[1]);
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long file_size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    unsigned char *data = malloc(file_size);
    if (!data) {
        fprintf(stderr, "Failed to allocate memory\n");
        fclose(fp);
        return 1;
    }

    if (fread(data, 1, file_size, fp) != file_size) {
        fprintf(stderr, "Failed to read file\n");
        free(data);
        fclose(fp);
        return 1;
    }

    fclose (fp);

    gif_t gif;
    if (gif_parse_gif(data, file_size, &gif)) {
        printf("GIF parsed successfully:\n");
        printf("Signature: %c%c%c\n", gif.signature[0], gif.signature[1], gif.signature[2]);
        printf("Version: %c%c%c\n", gif.version[0], gif.version[1], gif.version[2]);
        printf("Logical Screen Width: %d\n", gif.logical_screen_width);
        printf("Logical Screen Height: %d\n", gif.logical_screen_height);
        printf("Packed Fields: global_color_table_flag=%d, color_resolution=%d, sort_flag=%d, size_of_global_color_table=%d\n",
               gif.packed_fields.global_color_table_flag, gif.packed_fields.color_resolution, gif.packed_fields.sort_flag, gif.packed_fields.size_of_global_color_table);
        printf("Background Color Index: %d\n", gif.background_color_index);
        printf("Pixel Aspect Ratio: %d\n", gif.pixel_aspect_ratio);
        if (gif.global_color_table) {
            printf("Global Color Table: size=%d\n", 1 << gif.packed_fields.size_of_global_color_table);
        }
        gif_image_descriptor_t *image_descriptor = gif.image_descriptors;
        while (image_descriptor) {
            printf("Image Descriptor: separator=%c, left_position=%d, top_position=%d, width=%d, height=%d\n",
                   image_descriptor->separator, image_descriptor->left_position, image_descriptor->top_position, image_descriptor->width, image_descriptor->height);
            printf("Image Packed Fields: local_color_table_flag=%d, interlace_flag=%d, sort_flag=%d, reserved=%d, size_of_local_color_table=%d\n",
                   image_descriptor->packed_fields.local_color_table_flag, image_descriptor->packed_fields.interlace_flag, image_descriptor->packed_fields.sort_flag, image_descriptor->packed_fields.reserved, image_descriptor->packed_fields.size_of_local_color_table);
            if (image_descriptor->local_color_table) {
                printf("Local Color Table: size=%d\n", 1 << image_descriptor->packed_fields.size_of_local_color_table);
            }
            image_descriptor = image_descriptor->local_color_table;
        }
    } else {
        printf("Failed to parse GIF\n");
    }

    free(data);

    return 0;
}