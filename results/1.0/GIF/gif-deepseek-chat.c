#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define the GIF header structure
typedef struct {
    uint8_t signature[3]; // "GIF"
    uint8_t version[3];   // "87a" or "89a"
} GIFHeader;

// Define the GIF logical screen descriptor
typedef struct {
    uint16_t width;
    uint16_t height;
    uint8_t packed_fields;
    uint8_t background_color_index;
    uint8_t pixel_aspect_ratio;
} GIFLogicalScreenDescriptor;

// Define the GIF color table entry
typedef struct {
    uint8_t red;
    uint8_t green;
    uint8_t blue;
} GIFColorTableEntry;

// Define the GIF image descriptor
typedef struct {
    uint16_t left;
    uint16_t top;
    uint16_t width;
    uint16_t height;
    uint8_t packed_fields;
} GIFImageDescriptor;

// Define the GIF graphic control extension
typedef struct {
    uint8_t block_size;
    uint8_t packed_fields;
    uint16_t delay_time;
    uint8_t transparent_color_index;
} GIFGraphicControlExtension;

// Define the GIF application extension
typedef struct {
    uint8_t block_size;
    uint8_t application_identifier[8];
    uint8_t application_authentication_code[3];
} GIFApplicationExtension;

// Define the GIF comment extension
typedef struct {
    uint8_t block_size;
    uint8_t *comment_data;
} GIFCommentExtension;

// Define the GIF plain text extension
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
    uint8_t *plain_text_data;
} GIFPlainTextExtension;

// Define the GIF trailer
typedef struct {
    uint8_t trailer;
} GIFTrailer;

// Define the Hammer parser for the GIF header
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

// Define the Hammer parser for the GIF logical screen descriptor
HParser *gif_logical_screen_descriptor_parser() {
    return h_sequence(
        h_uint16(), // Width
        h_uint16(), // Height
        h_uint8(),  // Packed fields
        h_uint8(),  // Background color index
        h_uint8(),  // Pixel aspect ratio
        NULL
    );
}

// Define the Hammer parser for the GIF color table entry
HParser *gif_color_table_entry_parser() {
    return h_sequence(
        h_uint8(), // Red
        h_uint8(), // Green
        h_uint8(), // Blue
        NULL
    );
}

// Define the Hammer parser for the GIF image descriptor
HParser *gif_image_descriptor_parser() {
    return h_sequence(
        h_uint16(), // Left
        h_uint16(), // Top
        h_uint16(), // Width
        h_uint16(), // Height
        h_uint8(),  // Packed fields
        NULL
    );
}

// Define the Hammer parser for the GIF graphic control extension
HParser *gif_graphic_control_extension_parser() {
    return h_sequence(
        h_uint8(),  // Block size
        h_uint8(),  // Packed fields
        h_uint16(), // Delay time
        h_uint8(),  // Transparent color index
        NULL
    );
}

// Define the Hammer parser for the GIF application extension
HParser *gif_application_extension_parser() {
    return h_sequence(
        h_uint8(),  // Block size
        h_bytes(8, NULL), // Application identifier
        h_bytes(3, NULL), // Application authentication code
        NULL
    );
}

// Define the Hammer parser for the GIF comment extension
HParser *gif_comment_extension_parser() {
    return h_sequence(
        h_uint8(),  // Block size
        h_many(h_uint8()), // Comment data
        NULL
    );
}

// Define the Hammer parser for the GIF plain text extension
HParser *gif_plain_text_extension_parser() {
    return h_sequence(
        h_uint8(),  // Block size
        h_uint16(), // Text grid left
        h_uint16(), // Text grid top
        h_uint16(), // Text grid width
        h_uint16(), // Text grid height
        h_uint8(),  // Character cell width
        h_uint8(),  // Character cell height
        h_uint8(),  // Text foreground color index
        h_uint8(),  // Text background color index
        h_many(h_uint8()), // Plain text data
        NULL
    );
}

// Define the Hammer parser for the GIF trailer
HParser *gif_trailer_parser() {
    return h_uint8(); // Trailer
}

// Define the main Hammer parser for the entire GIF file
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

int main() {
    // Initialize the Hammer parser
    HParser *parser = gif_parser();

    // Read the GIF file into memory
    FILE *file = fopen("input.gif", "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    uint8_t *data = (uint8_t *)malloc(file_size);
    fread(data, 1, file_size, file);
    fclose(file);

    // Parse the GIF file
    HParseResult *result = h_parse(parser, data, file_size);
    if (!result) {
        fprintf(stderr, "Failed to parse GIF file\n");
        free(data);
        return 1;
    }

    // Free the allocated memory
    free(data);
    h_parse_result_free(result);

    return 0;
}