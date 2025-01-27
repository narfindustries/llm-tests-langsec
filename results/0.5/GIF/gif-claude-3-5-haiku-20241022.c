#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define GIF file structure parsing
static HParser *gif_signature;
static HParser *gif_version;
static HParser *gif_logical_screen_descriptor;
static HParser *gif_color_table;
static HParser *gif_image_descriptor;
static HParser *gif_image_data;
static HParser *gif_parser;

// Semantic actions for parsing
static HClosure *parse_gif_signature(HParseResult *p, void *user_data) {
    const uint8_t *data = p->ast->token_data;
    if (memcmp(data, "GIF", 3) == 0) {
        return h_make_closure(p->ast, NULL);
    }
    return NULL;
}

static HClosure *parse_gif_version(HParseResult *p, void *user_data) {
    const uint8_t *data = p->ast->token_data;
    if (memcmp(data, "87a", 3) == 0 || memcmp(data, "89a", 3) == 0) {
        return h_make_closure(p->ast, NULL);
    }
    return NULL;
}

void setup_gif_parsers() {
    gif_signature = h_sequence(
        h_token("GIF", 3),
        h_end_p(),
        NULL
    );

    gif_version = h_choice(
        h_token("87a", 3),
        h_token("89a", 3),
        NULL
    );

    gif_logical_screen_descriptor = h_sequence(
        h_uint16(),   // Logical Screen Width
        h_uint16(),   // Logical Screen Height
        h_uint8(),    // Packed Fields
        h_uint8(),    // Background Color Index
        h_uint8(),    // Pixel Aspect Ratio
        NULL
    );

    gif_color_table = h_repeat_n(h_uint8(), 3, 256 * 3);

    gif_image_descriptor = h_sequence(
        h_token(",", 1),
        h_uint16(),   // Image Left Position
        h_uint16(),   // Image Top Position
        h_uint16(),   // Image Width
        h_uint16(),   // Image Height
        h_uint8(),    // Packed Fields
        NULL
    );

    gif_image_data = h_sequence(
        h_uint8(),    // LZW Minimum Code Size
        h_many(h_uint8()),  // Image Data Blocks
        NULL
    );

    gif_parser = h_sequence(
        gif_signature,
        gif_version,
        gif_logical_screen_descriptor,
        h_optional(gif_color_table),
        gif_image_descriptor,
        gif_image_data,
        NULL
    );
}

int main(int argc, char **argv) {
    h_init(&argc, &argv);
    setup_gif_parsers();

    if (argc < 2) {
        fprintf(stderr, "Usage: %s <gif_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    size_t read_size = fread(buffer, 1, file_size, file);
    fclose(file);

    if (read_size != file_size) {
        perror("File read error");
        free(buffer);
        return 1;
    }

    HParseResult *result = h_parse(gif_parser, buffer, read_size);
    if (result && result->ast) {
        printf("GIF file parsed successfully\n");
    } else {
        printf("GIF file parsing failed\n");
    }

    h_parse_result_free(result);
    free(buffer);
    return 0;
}