#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define GIF_SIGNATURE "GIF"
#define GIF87_VERSION "87a"
#define GIF89_VERSION "89a"

typedef enum {
    GIF_GLOBAL_COLOR_TABLE_FLAG_ON,
    GIF_GLOBAL_COLOR_TABLE_FLAG_OFF
} gif_global_color_table_flag_t;

typedef enum {
    GIF_SORT_FLAG_ON,
    GIF_SORT_FLAG_OFF
} gif_sort_flag_t;

typedef enum {
    GIF_INTERLACE_FLAG_ON,
    GIF_INTERLACE_FLAG_OFF
} gif_interlace_flag_t;

typedef struct {
    char signature[3];
    char version[3];
    uint16_t width;
    uint16_t height;
    uint8_t packed_fields;
    uint8_t background_color_index;
    uint8_t pixel_aspect_ratio;
} gif_header_t;

typedef struct {
    uint8_t red;
    uint8_t green;
    uint8_t blue;
} gif_color_t;

typedef struct {
    uint8_t size;
    gif_color_t *colors;
} gif_color_table_t;

typedef struct {
    uint8_t image_separator;
    uint16_t left;
    uint16_t top;
    uint16_t width;
    uint16_t height;
    uint8_t packed_fields;
} gif_image_descriptor_t;

typedef struct {
    uint8_t label;
    uint8_t *data;
    size_t length;
} gif_extension_t;

typedef struct {
    uint8_t graphics_control_label;
    uint8_t block_size;
    uint8_t packed_fields;
    uint16_t delay_time;
    uint8_t transparent_color_index;
    uint8_t terminator;
} gif_graphics_control_extension_t;

typedef struct {
    uint8_t comment_label;
    uint8_t *data;
    size_t length;
} gif_comment_extension_t;

typedef struct {
    uint8_t application_label;
    uint8_t *data;
    size_t length;
} gif_application_extension_t;

void gif_signature(void *out) {
    char *signature = (char *)out;
    signature[0] = 'G';
    signature[1] = 'I';
    signature[2] = 'F';
}

void gif_version(void *out) {
    char *version = (char *)out;
    version[0] = '8';
    version[1] = '7';
    version[2] = 'a';
}

void gif_header(void *out, uint16_t width, uint16_t height, uint8_t packed_fields, uint8_t background_color_index, uint8_t pixel_aspect_ratio) {
    gif_header_t *header = (gif_header_t *)out;
    header->width = width;
    header->height = height;
    header->packed_fields = packed_fields;
    header->background_color_index = background_color_index;
    header->pixel_aspect_ratio = pixel_aspect_ratio;
}

void gif_color_table(void *out, uint8_t size, gif_color_t *colors) {
    gif_color_table_t *table = (gif_color_table_t *)out;
    table->size = size;
    table->colors = colors;
}

void gif_image_descriptor(void *out, uint8_t image_separator, uint16_t left, uint16_t top, uint16_t width, uint16_t height, uint8_t packed_fields) {
    gif_image_descriptor_t *descriptor = (gif_image_descriptor_t *)out;
    descriptor->image_separator = image_separator;
    descriptor->left = left;
    descriptor->top = top;
    descriptor->width = width;
    descriptor->height = height;
    descriptor->packed_fields = packed_fields;
}

void gif_extension(void *out, uint8_t label, uint8_t *data, size_t length) {
    gif_extension_t *extension = (gif_extension_t *)out;
    extension->label = label;
    extension->data = data;
    extension->length = length;
}

void gif_graphics_control_extension(void *out, uint8_t graphics_control_label, uint8_t block_size, uint8_t packed_fields, uint16_t delay_time, uint8_t transparent_color_index, uint8_t terminator) {
    gif_graphics_control_extension_t *extension = (gif_graphics_control_extension_t *)out;
    extension->graphics_control_label = graphics_control_label;
    extension->block_size = block_size;
    extension->packed_fields = packed_fields;
    extension->delay_time = delay_time;
    extension->transparent_color_index = transparent_color_index;
    extension->terminator = terminator;
}

void gif_comment_extension(void *out, uint8_t comment_label, uint8_t *data, size_t length) {
    gif_comment_extension_t *extension = (gif_comment_extension_t *)out;
    extension->comment_label = comment_label;
    extension->data = data;
    extension->length = length;
}

void gif_application_extension(void *out, uint8_t application_label, uint8_t *data, size_t length) {
    gif_application_extension_t *extension = (gif_application_extension_t *)out;
    extension->application_label = application_label;
    extension->data = data;
    extension->length = length;
}

gif_header_t gif_header_parser() {
    gif_header_t header;
    header.signature[0] = 'G';
    header.signature[1] = 'I';
    header.signature[2] = 'F';
    header.version[0] = '8';
    header.version[1] = '7';
    header.version[2] = 'a';
    return header;
}

gif_color_table_t gif_color_table_parser(uint8_t size, gif_color_t *colors) {
    gif_color_table_t table;
    table.size = size;
    table.colors = colors;
    return table;
}

gif_image_descriptor_t gif_image_descriptor_parser() {
    gif_image_descriptor_t descriptor;
    return descriptor;
}

gif_extension_t gif_extension_parser() {
    gif_extension_t extension;
    return extension;
}

gif_graphics_control_extension_t gif_graphics_control_extension_parser() {
    gif_graphics_control_extension_t extension;
    return extension;
}

gif_comment_extension_t gif_comment_extension_parser() {
    gif_comment_extension_t extension;
    return extension;
}

gif_application_extension_t gif_application_extension_parser() {
    gif_application_extension_t extension;
    return extension;
}

HAMMER_PARSER(gif_signature) {
    char signature[3];
    gif_signature(signature);
    HAMMER_TRY(hammer_bytes_of_length(signature, 3));
    return signature;
}

HAMMER_PARSER(gif_version) {
    char version[3];
    gif_version(version);
    HAMMER_TRY(hammer_bytes_of_length(version, 3));
    return version;
}

HAMMER_PARSER(gif_header) {
    gif_header_t header = gif_header_parser();
    HAMMER_TRY(hammer_sequence(
        hammer_bytes_of_length(header.signature, 3),
        hammer_bytes_of_length(header.version, 3),
        hammer_uint16_le(header.width),
        hammer_uint16_le(header.height),
        hammer_uint8(header.packed_fields),
        hammer_uint8(header.background_color_index),
        hammer_uint8(header.pixel_aspect_ratio)
    ));
    return header;
}

HAMMER_PARSER(gif_color_table) {
    uint8_t size;
    HAMMER_TRY(hammer_uint8());
    gif_color_t *colors = malloc(size * sizeof(gif_color_t));
    HAMMER_TRY(hammer_array_of(size, gif_color_t, sizeof(gif_color_t)));
    gif_color_table_t table = gif_color_table_parser(size, colors);
    return table;
}

HAMMER_PARSER(gif_image_descriptor) {
    gif_image_descriptor_t descriptor = gif_image_descriptor_parser();
    HAMMER_TRY(hammer_sequence(
        hammer_uint8(),
        hammer_uint16_le(descriptor.left),
        hammer_uint16_le(descriptor.top),
        hammer_uint16_le(descriptor.width),
        hammer_uint16_le(descriptor.height),
        hammer_uint8(descriptor.packed_fields)
    ));
    return descriptor;
}

HAMMER_PARSER(gif_extension) {
    uint8_t label = HAMMER_TRY(hammer_uint8());
    gif_extension_t extension = gif_extension_parser();
    if (label == 0xF9) {
        gif_graphics_control_extension_t graphics_control_extension = gif_graphics_control_extension_parser();
        HAMMER_TRY(hammer_sequence(
            hammer_uint8(graphics_control_extension.graphics_control_label),
            hammer_uint8(graphics_control_extension.block_size),
            hammer_uint8(graphics_control_extension.packed_fields),
            hammer_uint16_le(graphics_control_extension.delay_time),
            hammer_uint8(graphics_control_extension.transparent_color_index),
            hammer_uint8(graphics_control_extension.terminator)
        ));
    } else if (label == 0xFE) {
        gif_comment_extension_t comment_extension = gif_comment_extension_parser();
        HAMMER_TRY(hammer_sequence(
            hammer_uint8(comment_extension.comment_label),
            hammer_bytes()
        ));
    } else if (label == 0xFF) {
        gif_application_extension_t application_extension = gif_application_extension_parser();
        HAMMER_TRY(hammer_sequence(
            hammer_uint8(application_extension.application_label),
            hammer_bytes()
        ));
    }
    return extension;
}

HAMMER_PARSER(gif_image_data) {
    uint8_t length = HAMMER_TRY(hammer_uint8());
    HAMMER_TRY(hammer_bytes_of_length(length));
    return length;
}

HAMMER_PARSER(gif_trailer) {
    HAMMER_TRY(hammer_uint8());
    return 0x3B;
}

HAMMER_PARSER(gif) {
    gif_header_t header = HAMMER_TRY(gif_header());
    if (header.packed_fields & 0x80) {
        HAMMER_TRY(gif_color_table());
    }
    while (1) {
        HAMMER_TRY(hammer_or(
            HAMMER_TRY(gif_image_descriptor()),
            HAMMER_TRY(gif_extension())
        ));
    }
    HAMMER_TRY(gif_trailer());
    return header;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        printf("Usage: %s <file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_length = ftell(file);
    rewind(file);

    uint8_t *data = malloc(file_length);
    if (!data) {
        printf("Error allocating memory\n");
        return 1;
    }

    size_t bytes_read = fread(data, 1, file_length, file);
    if (bytes_read != file_length) {
        printf("Error reading file\n");
        return 1;
    }

    fclose(file);

    hammer_parser_t *parser = hammer_parser_new(gif());
    hammer_result_t result = hammer_parse(parser, data, file_length);

    if (result.error) {
        printf("Error parsing file: %s\n", result.error);
    } else {
        printf("File parsed successfully\n");
    }

    free(data);
    return 0;
}