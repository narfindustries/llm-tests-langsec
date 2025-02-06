#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    HParser* signature;
    HParser* version;
    HParser* screen_width;
    HParser* screen_height;
    HParser* packed_fields;
    HParser* background_color;
    HParser* pixel_aspect_ratio;
    HParser* global_color_table;
} GifHeader;

typedef struct {
    HParser* image_separator;
    HParser* left_position;
    HParser* top_position;
    HParser* width;
    HParser* height;
    HParser* packed_fields;
    HParser* local_color_table;
    HParser* image_data;
} ImageDescriptor;

typedef struct {
    HParser* extension_introducer;
    HParser* extension_type;
    HParser* extension_data;
} ExtensionBlock;

HParser* gif_signature() {
    return h_literal(h_ch('G'), h_ch('I'), h_ch('F'));
}

HParser* gif_version() {
    return h_choice(
        h_literal(h_ch('8'), h_ch('7'), h_ch('a')),
        h_literal(h_ch('8'), h_ch('9'), h_ch('a')),
        NULL
    );
}

HParser* color_table() {
    return h_repeat_n(
        h_sequence(
            h_uint8(),   // Red
            h_uint8(),   // Green
            h_uint8(),   // Blue
            NULL
        ),
        256
    );
}

HParser* gif_header() {
    return h_sequence(
        gif_signature(),
        gif_version(),
        h_uint16(),    // Screen width
        h_uint16(),    // Screen height
        h_uint8(),     // Packed fields
        h_uint8(),     // Background color index
        h_uint8(),     // Pixel aspect ratio
        NULL
    );
}

HParser* image_descriptor() {
    return h_sequence(
        h_literal(h_ch(0x2C)),  // Image separator
        h_uint16(),  // Left position
        h_uint16(),  // Top position
        h_uint16(),  // Width
        h_uint16(),  // Height
        h_uint8(),   // Packed fields
        NULL
    );
}

HParser* extension_block() {
    return h_sequence(
        h_literal(h_ch(0x21)),  // Extension introducer
        h_uint8(),  // Extension type
        NULL
    );
}

HParser* gif_parser() {
    return h_sequence(
        gif_header(),
        h_optional(color_table()),  // Global color table
        h_many(
            h_choice(
                image_descriptor(),
                extension_block(),
                NULL
            )
        ),
        h_literal(h_ch(0x3B)),  // Trailer
        NULL
    );
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
        perror("Error reading file");
        free(buffer);
        fclose(file);
        return 1;
    }
    fclose(file);

    HParser* parser = gif_parser();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result && result->ast) {
        printf("GIF file parsed successfully\n");
    } else {
        printf("Parsing failed\n");
    }

    h_parse_result_free(result);
    h_destroy_parser(parser);
    free(buffer);

    return 0;
}