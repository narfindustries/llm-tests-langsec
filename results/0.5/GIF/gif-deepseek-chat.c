#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define the GIF header structure
typedef struct {
    uint8_t signature[3]; // "GIF"
    uint8_t version[3];   // "87a" or "89a"
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
    uint8_t application_identifier[8];
    uint8_t application_authentication_code[3];
} GIFApplicationExtension;

// Define the GIF Comment Extension
typedef struct {
    uint8_t block_size;
    uint8_t comment_data[256]; // Variable length, max 256 bytes
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
    uint8_t plain_text_data[256]; // Variable length, max 256 bytes
} GIFPlainTextExtension;

// Define the GIF Trailer
typedef struct {
    uint8_t trailer; // 0x3B
} GIFTrailer;

// Hammer parser for GIFHeader
HParser *gif_header_parser() {
    return h_sequence(
        h_bytes(3, "GIF"), // Signature
        h_choice(
            h_bytes(3, "87a"),
            h_bytes(3, "89a"),
            NULL
        ), // Version
        NULL
    );
}

// Hammer parser for GIFLogicalScreenDescriptor
HParser *gif_logical_screen_descriptor_parser() {
    return h_sequence(
        h_int16(), // Width
        h_int16(), // Height
        h_uint8(), // Packed Fields
        h_uint8(), // Background Color Index
        h_uint8(), // Pixel Aspect Ratio
        NULL
    );
}

// Hammer parser for GIFColorTableEntry
HParser *gif_color_table_entry_parser() {
    return h_sequence(
        h_uint8(), // Red
        h_uint8(), // Green
        h_uint8(), // Blue
        NULL
    );
}

// Hammer parser for GIFImageDescriptor
HParser *gif_image_descriptor_parser() {
    return h_sequence(
        h_int16(), // Left
        h_int16(), // Top
        h_int16(), // Width
        h_int16(), // Height
        h_uint8(), // Packed Fields
        NULL
    );
}

// Hammer parser for GIFGraphicControlExtension
HParser *gif_graphic_control_extension_parser() {
    return h_sequence(
        h_uint8(), // Block Size
        h_uint8(), // Packed Fields
        h_int16(), // Delay Time
        h_uint8(), // Transparent Color Index
        NULL
    );
}

// Hammer parser for GIFApplicationExtension
HParser *gif_application_extension_parser() {
    return h_sequence(
        h_uint8(), // Block Size
        h_bytes(8, NULL), // Application Identifier
        h_bytes(3, NULL), // Application Authentication Code
        NULL
    );
}

// Hammer parser for GIFCommentExtension
HParser *gif_comment_extension_parser() {
    return h_sequence(
        h_uint8(), // Block Size
        h_bytes(256, NULL), // Comment Data
        NULL
    );
}

// Hammer parser for GIFPlainTextExtension
HParser *gif_plain_text_extension_parser() {
    return h_sequence(
        h_uint8(), // Block Size
        h_int16(), // Text Grid Left
        h_int16(), // Text Grid Top
        h_int16(), // Text Grid Width
        h_int16(), // Text Grid Height
        h_uint8(), // Character Cell Width
        h_uint8(), // Character Cell Height
        h_uint8(), // Text Foreground Color Index
        h_uint8(), // Text Background Color Index
        h_bytes(256, NULL), // Plain Text Data
        NULL
    );
}

// Hammer parser for GIFTrailer
HParser *gif_trailer_parser() {
    return h_uint8(); // Trailer (0x3B)
}

// Main GIF parser
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

    HParser *parser = gif_parser();
    HParseResult *result = h_parse(parser, data, file_size);

    if (result) {
        printf("Parsing succeeded!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }

    free(data);
    return 0;
}