#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define parsers for various components of the GIF format
HParser *gif_signature;
HParser *gif_version;
HParser *logical_screen_descriptor;
HParser *global_color_table;
HParser *image_descriptor;
HParser *local_color_table;
HParser *image_data;
HParser *graphic_control_extension;
HParser *comment_extension;
HParser *plain_text_extension;
HParser *application_extension;
HParser *gif_file;

void init_parsers() {
    // Basic components
    gif_signature = h_token("GIF", 3);
    gif_version = h_choice(h_token("89a", 3), h_token("87a", 3), NULL);

    // Logical Screen Descriptor
    logical_screen_descriptor = h_sequence(
        h_uint16(), // Logical Screen Width
        h_uint16(), // Logical Screen Height
        h_bits(8, false), // Packed Fields
        h_uint8(), // Background Color Index
        h_uint8(), // Pixel Aspect Ratio
        NULL
    );

    // Global Color Table
    global_color_table = h_many(h_bits(24, false)); // RGB entries, 3 bytes each

    // Image Descriptor
    image_descriptor = h_sequence(
        h_uint8(), // Image Separator
        h_uint16(), // Image Left Position
        h_uint16(), // Image Top Position
        h_uint16(), // Image Width
        h_uint16(), // Image Height
        h_bits(8, false), // Packed Fields
        NULL
    );

    // Local Color Table
    local_color_table = h_many(h_bits(24, false)); // RGB entries, 3 bytes each

    // Image Data
    image_data = h_sequence(
        h_uint8(), // LZW Minimum Code Size
        h_many(h_length_value(h_uint8(), h_greedy_bytes())), // Sub-blocks
        NULL
    );

    // Graphic Control Extension
    graphic_control_extension = h_sequence(
        h_uint8(), // Extension Introducer
        h_uint8(), // Graphic Control Label
        h_uint8(), // Block Size
        h_bits(8, false), // Packed Fields
        h_uint16(), // Delay Time
        h_uint8(), // Transparent Color Index
        h_uint8(), // Block Terminator
        NULL
    );

    // Comment Extension
    comment_extension = h_sequence(
        h_uint8(), // Extension Introducer
        h_uint8(), // Comment Label
        h_many(h_length_value(h_uint8(), h_greedy_bytes())), // Comment Data
        NULL
    );

    // Plain Text Extension
    plain_text_extension = h_sequence(
        h_uint8(), // Extension Introducer
        h_uint8(), // Plain Text Label
        h_uint8(), // Block Size
        h_uint16(), h_uint16(), h_uint16(), h_uint16(), // Text Grid
        h_uint8(), h_uint8(), // Character Width, Height
        h_uint8(), h_uint8(), // Text Foreground, Background Color Index
        h_many(h_length_value(h_uint8(), h_greedy_bytes())), // Text Data
        NULL
    );

    // Application Extension
    application_extension = h_sequence(
        h_uint8(), // Extension Introducer
        h_uint8(), // Application Extension Label
        h_uint8(), // Block Size
        h_bytes(8), // Application Identifier
        h_bytes(3), // Application Authentication Code
        h_many(h_length_value(h_uint8(), h_greedy_bytes())), // Application Data
        NULL
    );

    // GIF File
    gif_file = h_sequence(
        gif_signature,
        gif_version,
        logical_screen_descriptor,
        h_optional(global_color_table),
        h_many(h_choice(
            image_descriptor,
            graphic_control_extension,
            comment_extension,
            plain_text_extension,
            application_extension,
            NULL
        )),
        h_uint8(), // Trailer
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

    fread(data, 1, size, fp);
    fclose(fp);

    init_parsers();

    HParseResult *result = h_parse(gif_file, data, size);
    if (result) {
        printf("GIF parsed successfully!\n");
    } else {
        printf("Failed to parse GIF.\n");
    }

    free(data);
    return 0;
}