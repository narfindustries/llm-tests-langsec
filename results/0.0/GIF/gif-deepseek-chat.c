#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define the GIF header structure
typedef struct {
    char signature[3]; // "GIF"
    char version[3];   // "87a" or "89a"
} GIFHeader;

// Define the GIF Logical Screen Descriptor
typedef struct {
    uint16_t width;
    uint16_t height;
    uint8_t packed_fields;
    uint8_t background_color_index;
    uint8_t pixel_aspect_ratio;
} GIFLogicalScreenDescriptor;

// Define the GIF Color Table Entry
typedef struct {
    uint8_t red;
    uint8_t green;
    uint8_t blue;
} GIFColorTableEntry;

// Define the GIF Image Descriptor
typedef struct {
    uint16_t left;
    uint16_t top;
    uint16_t width;
    uint16_t height;
    uint8_t packed_fields;
} GIFImageDescriptor;

// Define the GIF Graphic Control Extension
typedef struct {
    uint8_t block_size;
    uint8_t packed_fields;
    uint16_t delay_time;
    uint8_t transparent_color_index;
} GIFGraphicControlExtension;

// Define the GIF Application Extension
typedef struct {
    uint8_t block_size;
    char application_identifier[8];
    char authentication_code[3];
} GIFApplicationExtension;

// Define the GIF Comment Extension
typedef struct {
    uint8_t block_size;
    char *comment_data;
} GIFCommentExtension;

// Define the GIF Plain Text Extension
typedef struct {
    uint8_t block_size;
    uint16_t text_grid_left;
    uint16_t text_grid_top;
    uint16_t text_grid_width;
    uint16_t text_grid_height;
    uint8_t character_cell_width;
    uint8_t character_cell_height;
    uint8_t text_foreground_color_index;
    uint8_t text_background_color_index;
    char *plain_text_data;
} GIFPlainTextExtension;

// Define the GIF Trailer
typedef struct {
    uint8_t trailer;
} GIFTrailer;

// Define the Hammer parser for the GIF header
HParser *gif_header_parser() {
    return h_sequence(
        h_chars("GIF", 3),
        h_chars("87a", 3),
        NULL
    );
}

// Define the Hammer parser for the GIF Logical Screen Descriptor
HParser *gif_logical_screen_descriptor_parser() {
    return h_sequence(
        h_int16(),
        h_int16(),
        h_uint8(),
        h_uint8(),
        h_uint8(),
        NULL
    );
}

// Define the Hammer parser for the GIF Color Table Entry
HParser *gif_color_table_entry_parser() {
    return h_sequence(
        h_uint8(),
        h_uint8(),
        h_uint8(),
        NULL
    );
}

// Define the Hammer parser for the GIF Image Descriptor
HParser *gif_image_descriptor_parser() {
    return h_sequence(
        h_int16(),
        h_int16(),
        h_int16(),
        h_int16(),
        h_uint8(),
        NULL
    );
}

// Define the Hammer parser for the GIF Graphic Control Extension
HParser *gif_graphic_control_extension_parser() {
    return h_sequence(
        h_uint8(),
        h_uint8(),
        h_int16(),
        h_uint8(),
        NULL
    );
}

// Define the Hammer parser for the GIF Application Extension
HParser *gif_application_extension_parser() {
    return h_sequence(
        h_uint8(),
        h_chars(NULL, 8),
        h_chars(NULL, 3),
        NULL
    );
}

// Define the Hammer parser for the GIF Comment Extension
HParser *gif_comment_extension_parser() {
    return h_sequence(
        h_uint8(),
        h_many(h_uint8()),
        NULL
    );
}

// Define the Hammer parser for the GIF Plain Text Extension
HParser *gif_plain_text_extension_parser() {
    return h_sequence(
        h_uint8(),
        h_int16(),
        h_int16(),
        h_int16(),
        h_int16(),
        h_uint8(),
        h_uint8(),
        h_uint8(),
        h_uint8(),
        h_many(h_uint8()),
        NULL
    );
}

// Define the Hammer parser for the GIF Trailer
HParser *gif_trailer_parser() {
    return h_sequence(
        h_uint8(),
        NULL
    );
}

// Define the main Hammer parser for the GIF file
HParser *gif_parser() {
    return h_sequence(
        gif_header_parser(),
        gif_logical_screen_descriptor_parser(),
        h_many(gif_color_table_entry_parser()),
        h_many(h_choice(
            gif_image_descriptor_parser(),
            gif_graphic_control_extension_parser(),
            gif_application_extension_parser(),
            gif_comment_extension_parser(),
            gif_plain_text_extension_parser(),
            NULL
        )),
        gif_trailer_parser(),
        NULL
    );
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <gif file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(gif_parser(), data, file_size);
    if (!result) {
        fprintf(stderr, "Failed to parse GIF file\n");
        free(data);
        return 1;
    }

    printf("Successfully parsed GIF file\n");

    h_parse_result_free(result);
    free(data);

    return 0;
}