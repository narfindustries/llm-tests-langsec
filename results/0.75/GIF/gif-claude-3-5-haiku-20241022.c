#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    char signature[3];
    char version[3];
    uint16_t screen_width;
    uint16_t screen_height;
    uint8_t packed_fields;
    uint8_t background_color_index;
    uint8_t pixel_aspect_ratio;
    HParseResult* global_color_table;
} GifHeader;

typedef struct {
    uint8_t separator;
    uint16_t left_position;
    uint16_t top_position;
    uint16_t image_width;
    uint16_t image_height;
    uint8_t packed_fields;
    HParseResult* local_color_table;
    HParseResult* image_data;
} ImageDescriptor;

typedef struct {
    uint8_t extension_introducer;
    uint8_t graphic_control_label;
    uint8_t block_size;
    uint8_t packed_fields;
    uint16_t delay_time;
    uint8_t transparent_color_index;
} GraphicControlExtension;

HParseResult* parse_gif_header(const uint8_t* input, size_t len) {
    HParser* signature = h_token((uint8_t*)"GIF", 3);
    HParser* version = h_choice(h_token((uint8_t*)"87a", 3), h_token((uint8_t*)"89a", 3), NULL);
    HParser* screen_width = h_int_range(h_uint8(), 0, 65535);
    HParser* screen_height = h_int_range(h_uint8(), 0, 65535);
    HParser* packed_fields = h_uint8();
    HParser* background_color_index = h_uint8();
    HParser* pixel_aspect_ratio = h_uint8();
    HParser* global_color_table = h_many(h_bits(24, false));

    HParser* gif_header = h_sequence(signature, version, screen_width, screen_height, 
                                     packed_fields, background_color_index, 
                                     pixel_aspect_ratio, global_color_table, NULL);
    return h_parse(gif_header, input, len);
}

HParseResult* parse_image_descriptor(const uint8_t* input, size_t len) {
    HParser* separator = h_token((uint8_t*)"\x2C", 1);
    HParser* left_position = h_int_range(h_uint8(), 0, 65535);
    HParser* top_position = h_int_range(h_uint8(), 0, 65535);
    HParser* image_width = h_int_range(h_uint8(), 0, 65535);
    HParser* image_height = h_int_range(h_uint8(), 0, 65535);
    HParser* packed_fields = h_uint8();
    HParser* local_color_table = h_many(h_bits(24, false));
    HParser* image_data = h_many(h_uint8());

    HParser* image_descriptor = h_sequence(separator, left_position, top_position, 
                                           image_width, image_height, packed_fields, 
                                           local_color_table, image_data, NULL);
    return h_parse(image_descriptor, input, len);
}

HParseResult* parse_graphic_control_extension(const uint8_t* input, size_t len) {
    HParser* extension_introducer = h_token((uint8_t*)"\x21", 1);
    HParser* graphic_control_label = h_token((uint8_t*)"\xF9", 1);
    HParser* block_size = h_token((uint8_t*)"\x04", 1);
    HParser* packed_fields = h_uint8();
    HParser* delay_time = h_int_range(h_uint8(), 0, 65535);
    HParser* transparent_color_index = h_uint8();

    HParser* graphic_control = h_sequence(extension_introducer, graphic_control_label, 
                                          block_size, packed_fields, delay_time, 
                                          transparent_color_index, NULL);
    return h_parse(graphic_control, input, len);
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gif_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation failed");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("File read error");
        free(buffer);
        fclose(file);
        return 1;
    }
    fclose(file);

    HParseResult* header = parse_gif_header(buffer, file_size);
    HParseResult* image = parse_image_descriptor(buffer, file_size);
    HParseResult* graphic_ext = parse_graphic_control_extension(buffer, file_size);

    if (header && image && graphic_ext) {
        printf("GIF parsed successfully\n");
    } else {
        printf("Parsing failed\n");
    }

    if (header) h_parse_result_free(header);
    if (image) h_parse_result_free(image);
    if (graphic_ext) h_parse_result_free(graphic_ext);

    free(buffer);
    return 0;
}