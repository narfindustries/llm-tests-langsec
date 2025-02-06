#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Parser for a single RGB entry in the color table
static HParser *rgb_parser() {
    return h_sequence(h_uint8(), h_uint8(), h_uint8(), NULL);
}

// Parser for the color table
static HParser *color_table_parser(int size) {
    return h_repeat_n(rgb_parser(), size);
}

// Parser for the Logical Screen Descriptor
static HParser *logical_screen_descriptor_parser() {
    return h_sequence(
        h_uint16(),  // Logical Screen Width
        h_uint16(),  // Logical Screen Height
        h_bits(8, false),   // Packed Fields
        h_uint8(),   // Background Color Index
        h_uint8(),   // Pixel Aspect Ratio
        NULL
    );
}

// Parser for the Image Descriptor
static HParser *image_descriptor_parser() {
    return h_sequence(
        h_uint16(),  // Image Left Position
        h_uint16(),  // Image Top Position
        h_uint16(),  // Image Width
        h_uint16(),  // Image Height
        h_bits(8, false),   // Packed Fields
        NULL
    );
}

// Parser for the Graphic Control Extension
static HParser *graphic_control_extension_parser() {
    return h_sequence(
        h_uint8(),   // Block Size
        h_bits(8, false),   // Packed Fields
        h_uint16(),  // Delay Time
        h_uint8(),   // Transparent Color Index
        h_uint8(),   // Block Terminator
        NULL
    );
}

// Parser for the Application Extension
static HParser *application_extension_parser() {
    return h_sequence(
        h_bytes(8),  // Application Identifier
        h_bytes(3),  // Application Authentication Code
        h_length_value(h_uint8(), h_bytes()),  // Application Data Sub-blocks
        NULL
    );
}

// Parser for the Comment Extension
static HParser *comment_extension_parser() {
    return h_length_value(h_uint8(), h_bytes());  // Comment Data Sub-blocks
}

// Parser for the Plain Text Extension
static HParser *plain_text_extension_parser() {
    return h_sequence(
        h_uint8(),    // Block Size
        h_uint16(),   // Text Grid Left Position
        h_uint16(),   // Text Grid Top Position
        h_uint16(),   // Text Grid Width
        h_uint16(),   // Text Grid Height
        h_uint8(),    // Character Width
        h_uint8(),    // Character Height
        h_uint8(),    // Text Foreground Color Index
        h_uint8(),    // Text Background Color Index
        h_length_value(h_uint8(), h_bytes()),  // Text Data Sub-blocks
        NULL
    );
}

// Main parser for the GIF file format
static HParser *gif_parser() {
    return h_sequence(
        h_string("GIF", 3),  // Signature
        h_string("89a", 3),  // Version
        logical_screen_descriptor_parser(),
        h_optional(color_table_parser(256)),  // Optional Global Color Table
        h_many(h_choice(  // Blocks
            image_descriptor_parser(),
            graphic_control_extension_parser(),
            application_extension_parser(),
            comment_extension_parser(),
            plain_text_extension_parser(),
            NULL
        )),
        h_uint8(),  // Trailer
        NULL
    );
}

int main(int argc, char *argv[]) {
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
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buffer = malloc(fsize);
    fread(buffer, 1, fsize, fp);
    fclose(fp);

    HParser *parser = gif_parser();
    HParseResult *result = h_parse(parser, buffer, fsize);
    if (result) {
        printf("Parse successful!\n");
    } else {
        printf("Parse failed.\n");
    }

    h_parse_result_free(result);
    free(buffer);
    return 0;
}