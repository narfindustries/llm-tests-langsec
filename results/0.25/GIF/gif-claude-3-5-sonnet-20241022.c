#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// GIF Parser using Hammer

// Helper parsers
static HParser* color_table(size_t size) {
    return h_repeat_n(h_uint8(), size * 3);
}

// Image Descriptor Parser
static HParser* image_descriptor() {
    return h_sequence(
        h_ch(','),                    // Image Separator
        h_uint16(),                   // Image Left Position
        h_uint16(),                   // Image Top Position
        h_uint16(),                   // Image Width
        h_uint16(),                   // Image Height
        h_bits(8, false),            // Packed Fields
        NULL
    );
}

// Graphics Control Extension Parser
static HParser* graphics_control_extension() {
    return h_sequence(
        h_ch('!'),                    // Extension Introducer
        h_ch(0xF9),                   // Graphics Control Label
        h_uint8(),                    // Block Size (4)
        h_bits(8, false),            // Packed Field
        h_uint16(),                   // Delay Time
        h_uint8(),                    // Transparent Color Index
        h_ch(0x00),                   // Block Terminator
        NULL
    );
}

// Comment Extension Parser
static HParser* comment_extension() {
    return h_sequence(
        h_ch('!'),                    // Extension Introducer
        h_ch(0xFE),                   // Comment Label
        h_many1(h_sequence(
            h_uint8(),                // Block Size
            h_uint8(),                // Comment Data
            NULL
        )),
        h_ch(0x00),                   // Block Terminator
        NULL
    );
}

// Plain Text Extension Parser
static HParser* plain_text_extension() {
    return h_sequence(
        h_ch('!'),                    // Extension Introducer
        h_ch(0x01),                   // Plain Text Label
        h_uint8(),                    // Block Size (12)
        h_uint16(),                   // Text Grid Left Position
        h_uint16(),                   // Text Grid Top Position
        h_uint16(),                   // Text Grid Width
        h_uint16(),                   // Text Grid Height
        h_uint8(),                    // Character Cell Width
        h_uint8(),                    // Character Cell Height
        h_uint8(),                    // Text Foreground Color Index
        h_uint8(),                    // Text Background Color Index
        h_many1(h_sequence(
            h_uint8(),                // Block Size
            h_uint8(),                // Plain Text Data
            NULL
        )),
        h_ch(0x00),                   // Block Terminator
        NULL
    );
}

// Application Extension Parser
static HParser* application_extension() {
    return h_sequence(
        h_ch('!'),                    // Extension Introducer
        h_ch(0xFF),                   // Application Extension Label
        h_uint8(),                    // Block Size (11)
        h_repeat_n(h_uint8(), 8),     // Application Identifier
        h_repeat_n(h_uint8(), 3),     // Application Authentication Code
        h_many1(h_sequence(
            h_uint8(),                // Block Size
            h_uint8(),                // Application Data
            NULL
        )),
        h_ch(0x00),                   // Block Terminator
        NULL
    );
}

// Main GIF Parser
static HParser* gif_parser() {
    return h_sequence(
        h_token((const uint8_t*)"GIF", 3),    // Signature
        h_token((const uint8_t*)"87a", 3),    // Version
        h_uint16(),                           // Logical Screen Width
        h_uint16(),                           // Logical Screen Height
        h_bits(8, false),                    // Packed Fields
        h_uint8(),                           // Background Color Index
        h_uint8(),                           // Pixel Aspect Ratio
        h_optional(color_table(256)),         // Global Color Table
        h_many(h_choice(
            graphics_control_extension(),
            comment_extension(),
            plain_text_extension(),
            application_extension(),
            image_descriptor(),
            NULL
        )),
        h_ch(0x3B),                          // Trailer
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gif_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if (fread(data, 1, size, file) != size) {
        perror("Failed to read file");
        free(data);
        fclose(file);
        return 1;
    }

    HParser *gif = gif_parser();
    HParseResult *result = h_parse(gif, data, size);

    if (result) {
        printf("GIF parsing successful\n");
        h_parse_result_free(result);
    } else {
        printf("GIF parsing failed\n");
    }

    free(data);
    fclose(file);
    return 0;
}