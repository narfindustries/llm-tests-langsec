#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Parser for a single RGB color entry
static HParser *rgb_color() {
    return h_sequence(h_uint8(), h_uint8(), h_uint8(), NULL);
}

// Parser for the Global Color Table
static HParser *global_color_table(int size) {
    return h_repeat_n(rgb_color(), size);
}

// Parser for the Logical Screen Descriptor
static HParser *logical_screen_descriptor() {
    return h_sequence(
        h_uint16(), // Logical Screen Width
        h_uint16(), // Logical Screen Height
        h_bits(8, false),  // Packed Fields
        h_uint8(),  // Background Color Index
        h_uint8(),  // Pixel Aspect Ratio
        NULL
    );
}

// Parser for the Image Descriptor
static HParser *image_descriptor() {
    return h_sequence(
        h_uint8(),  // Image Separator
        h_uint16(), // Image Left Position
        h_uint16(), // Image Top Position
        h_uint16(), // Image Width
        h_uint16(), // Image Height
        h_bits(8, false),  // Packed Fields
        NULL
    );
}

// Parser for the Block Terminator
static HParser *block_terminator() {
    return h_uint8();
}

// Parser for the Graphic Control Extension
static HParser *graphic_control_extension() {
    return h_sequence(
        h_uint8(),  // Extension Introducer
        h_uint8(),  // Graphic Control Label
        h_uint8(),  // Block Size
        h_bits(8, false),  // Packed Fields
        h_uint16(), // Delay Time
        h_uint8(),  // Transparent Color Index
        h_uint8(),  // Block Terminator
        NULL
    );
}

// Parser for the Application Extension
static HParser *application_extension() {
    return h_sequence(
        h_uint8(),      // Extension Introducer
        h_uint8(),      // Extension Label
        h_uint8(),      // Block Size
        h_bits(64, false),     // Application Identifier
        h_bits(24, false),     // Application Authentication Code
        h_many(h_uint8()), // Application Data (sub-blocks)
        h_uint8(),      // Block Terminator
        NULL
    );
}

// Parser for the Comment Extension
static HParser *comment_extension() {
    return h_sequence(
        h_uint8(),      // Extension Introducer
        h_uint8(),      // Comment Label
        h_many(h_uint8()), // Comment Data (sub-blocks)
        h_uint8(),      // Block Terminator
        NULL
    );
}

// Parser for the Trailer
static HParser *trailer() {
    return h_uint8();
}

// Parser for the entire GIF file
static HParser *gif_parser() {
    return h_sequence(
        h_bytes("GIF", 3), // Signature
        h_bytes("89a", 3), // Version
        logical_screen_descriptor(),
        h_optional(global_color_table(256)), // Optional Global Color Table
        h_many(h_choice(
            image_descriptor(),
            graphic_control_extension(),
            application_extension(),
            comment_extension(),
            NULL
        )),
        trailer(),
        NULL
    );
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <GIF file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("File opening failed");
        return EXIT_FAILURE;
    }

    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *data = malloc(size);
    if (!data) {
        perror("Memory allocation failed");
        fclose(fp);
        return EXIT_FAILURE;
    }

    if (fread(data, 1, size, fp) != size) {
        perror("File reading failed");
        free(data);
        fclose(fp);
        return EXIT_FAILURE;
    }

    fclose(fp);

    HParser *parser = gif_parser();
    HParseResult *result = h_parse(parser, data, size);
    if (result) {
        printf("GIF parsed successfully.\n");
    } else {
        printf("Failed to parse GIF.\n");
    }

    h_parse_result_free(result);
    free(data);

    return 0;
}