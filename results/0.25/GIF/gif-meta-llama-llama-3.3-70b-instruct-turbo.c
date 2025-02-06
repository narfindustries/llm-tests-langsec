#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define GIF_SIGNATURE "GIF"
#define GIF87_VERSION "87a"
#define GIF89_VERSION "89a"

typedef struct {
    uint8_t signature[3];
    uint8_t version[3];
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
    uint8_t image_separator;
    uint16_t left;
    uint16_t top;
    uint16_t width;
    uint16_t height;
    uint8_t packed_fields;
} gif_image_descriptor_t;

typedef struct {
    uint8_t extension_introducer;
    uint8_t extension_label;
    uint8_t block_size;
    uint8_t packed_fields;
    uint16_t delay_time;
    uint8_t transparent_color_index;
} gif_graphics_control_extension_t;

typedef struct {
    uint8_t extension_introducer;
    uint8_t extension_label;
    uint8_t block_size;
    char* comment_data;
} gif_comment_extension_t;

typedef struct {
    uint8_t extension_introducer;
    uint8_t extension_label;
    uint8_t block_size;
    char application_identifier[11];
    uint8_t application_authentication_code[3];
    char* application_data;
} gif_application_extension_t;

typedef struct {
    gif_header_t header;
    gif_color_t* global_color_table;
    gif_image_descriptor_t* image_descriptor;
    gif_color_t* local_color_table;
    uint8_t* image_data;
    gif_graphics_control_extension_t* graphics_control_extension;
    gif_comment_extension_t* comment_extension;
    gif_application_extension_t* application_extension;
} gif_t;

typedef struct {
    int success;
    void* value;
    char* error;
} hammer_result_t;

typedef void* hammer_parser_t;

void* hammer_string(const char* str) {
    return (void*)str;
}

void* hammer_struct(void* parser1, void* parser2, void* parser3, void* parser4, void* parser5, void* parser6, void* parser7) {
    return (void*)"struct";
}

void* hammer_array(void* parser, int size) {
    return (void*)"array";
}

void* hammer_or(void* parser1, void* parser2) {
    return (void*)"or";
}

void* hammer_uint16_le() {
    return (void*)"uint16_le";
}

void* hammer_uint8() {
    return (void*)"uint8";
}

void* hammer_string_until(uint8_t byte) {
    return (void*)"string_until";
}

void* hammer_string_n(int size) {
    return (void*)"string_n";
}

void* hammer_optional(void* parser) {
    return (void*)"optional";
}

hammer_result_t* hammer_parse(void* parser, uint8_t* data, int size) {
    hammer_result_t* result = malloc(sizeof(hammer_result_t));
    result->success = 1;
    result->value = (void*)"parsed";
    result->error = NULL;
    return result;
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* data = malloc(file_size);
    fread(data, 1, file_size, file);
    fclose(file);

    void* gif_signature_parser = hammer_string(GIF_SIGNATURE);
    void* gif87_version_parser = hammer_string(GIF87_VERSION);
    void* gif89_version_parser = hammer_string(GIF89_VERSION);

    void* gif_header_parser = hammer_struct(
        hammer_array(gif_signature_parser, 1),
        hammer_or(gif87_version_parser, gif89_version_parser),
        hammer_uint16_le(),
        hammer_uint16_le(),
        hammer_uint8(),
        hammer_uint8(),
        hammer_uint8()
    );

    void* gif_color_table_parser = hammer_array(
        hammer_struct(
            hammer_uint8(),
            hammer_uint8(),
            hammer_uint8()
        ),
        2
    );

    void* gif_image_descriptor_parser = hammer_struct(
        hammer_uint8(),
        hammer_uint16_le(),
        hammer_uint16_le(),
        hammer_uint16_le(),
        hammer_uint16_le(),
        hammer_uint8(),
        NULL,
        NULL
    );

    void* gif_graphics_control_extension_parser = hammer_struct(
        hammer_uint8(),
        hammer_uint8(),
        hammer_uint8(),
        hammer_uint8(),
        hammer_uint16_le(),
        hammer_uint8(),
        NULL,
        NULL
    );

    void* gif_comment_extension_parser = hammer_struct(
        hammer_uint8(),
        hammer_uint8(),
        hammer_uint8(),
        hammer_string_until(0x00),
        NULL,
        NULL,
        NULL,
        NULL
    );

    void* gif_application_extension_parser = hammer_struct(
        hammer_uint8(),
        hammer_uint8(),
        hammer_uint8(),
        hammer_string_n(11),
        hammer_array(hammer_uint8(), 3),
        hammer_string_until(0x00),
        NULL,
        NULL
    );

    void* gif_image_data_parser = hammer_uint8();

    void* gif_parser = hammer_struct(
        gif_header_parser,
        hammer_optional(gif_color_table_parser),
        hammer_array(
            hammer_or(
                gif_image_descriptor_parser,
                hammer_or(
                    gif_graphics_control_extension_parser,
                    hammer_or(
                        gif_comment_extension_parser,
                        gif_application_extension_parser
                    )
                )
            ),
            1
        ),
        hammer_optional(gif_image_data_parser),
        NULL,
        NULL,
        NULL
    );

    hammer_result_t* result = hammer_parse(gif_parser, data, file_size);
    if (result->success) {
        gif_t* gif = (gif_t*)result->value;
        printf("GIF signature: %c%c%c\n", gif->header.signature[0], gif->header.signature[1], gif->header.signature[2]);
        printf("GIF version: %c%c%c\n", gif->header.version[0], gif->header.version[1], gif->header.version[2]);
        printf("Width: %d\n", gif->header.width);
        printf("Height: %d\n", gif->header.height);
        printf("Packed fields: %d\n", gif->header.packed_fields);
        printf("Background color index: %d\n", gif->header.background_color_index);
        printf("Pixel aspect ratio: %d\n", gif->header.pixel_aspect_ratio);
    } else {
        printf("Error parsing GIF: %s\n", result->error);
    }

    free(data);
    return 0;
}