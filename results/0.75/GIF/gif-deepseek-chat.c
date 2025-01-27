#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define the GIF header structure
typedef struct {
    uint8_t signature[3]; // "GIF"
    uint8_t version[3];   // "87a" or "89a"
} GIFHeader;

// Define the GIF Logical Screen Descriptor structure
typedef struct {
    uint16_t width;
    uint16_t height;
    uint8_t packed_fields;
    uint8_t background_color_index;
    uint8_t pixel_aspect_ratio;
} GIFLogicalScreenDescriptor;

// Define the GIF Color Table structure
typedef struct {
    uint8_t red;
    uint8_t green;
    uint8_t blue;
} GIFColorTableEntry;

// Define the GIF Image Descriptor structure
typedef struct {
    uint8_t separator; // 0x2C
    uint16_t left;
    uint16_t top;
    uint16_t width;
    uint16_t height;
    uint8_t packed_fields;
} GIFImageDescriptor;

// Define the GIF Trailer structure
typedef struct {
    uint8_t trailer; // 0x3B
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

// Define the Hammer parser for the GIF Logical Screen Descriptor
HParser *gif_logical_screen_descriptor_parser() {
    return h_sequence(
        h_uint16(), // Width
        h_uint16(), // Height
        h_uint8(),  // Packed Fields
        h_uint8(),  // Background Color Index
        h_uint8(),  // Pixel Aspect Ratio
        NULL
    );
}

// Define the Hammer parser for the GIF Color Table
HParser *gif_color_table_parser(size_t num_entries) {
    return h_repeat_n(h_sequence(
        h_uint8(), // Red
        h_uint8(), // Green
        h_uint8(), // Blue
        NULL
    ), num_entries);
}

// Define the Hammer parser for the GIF Image Descriptor
HParser *gif_image_descriptor_parser() {
    return h_sequence(
        h_uint8(),  // Separator (0x2C)
        h_uint16(), // Left
        h_uint16(), // Top
        h_uint16(), // Width
        h_uint16(), // Height
        h_uint8(),  // Packed Fields
        NULL
    );
}

// Define the Hammer parser for the GIF Trailer
HParser *gif_trailer_parser() {
    return h_uint8(); // Trailer (0x3B)
}

// Define the main GIF parser
HParser *gif_parser() {
    return h_sequence(
        gif_header_parser(),
        gif_logical_screen_descriptor_parser(),
        h_optional(gif_color_table_parser(256)), // Assuming 256 colors for simplicity
        h_repeat(h_sequence(
            gif_image_descriptor_parser(),
            h_optional(gif_color_table_parser(256)), // Assuming 256 colors for simplicity
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
        printf("GIF parsed successfully!\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse GIF.\n");
    }

    free(data);
    return 0;
}