#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define parsers for each part of the GIF format
HParser *header, *logical_screen_descriptor, *global_color_table, *image_descriptor;
HParser *local_color_table, *image_data, *extension, *trailer, *gif_file;

void init_parsers() {
    header = h_sequence(
        h_bytes((const uint8_t *)"GIF", 3),
        h_choice(h_bytes((const uint8_t *)"87a", 3), h_bytes((const uint8_t *)"89a", 3), NULL),
        NULL
    );

    logical_screen_descriptor = h_sequence(
        h_uint16(), // canvas width
        h_uint16(), // canvas height
        h_bits(8, false), // packed fields
        h_uint8(), // background color index
        h_uint8(), // pixel aspect ratio
        NULL
    );

    global_color_table = h_many(h_sequence(h_uint8(), h_uint8(), h_uint8(), NULL));

    image_descriptor = h_sequence(
        h_uint8(), // image separator
        h_uint16(), // image left position
        h_uint16(), // image top position
        h_uint16(), // image width
        h_uint16(), // image height
        h_bits(8, false), // packed fields
        NULL
    );

    local_color_table = h_many(h_sequence(h_uint8(), h_uint8(), h_uint8(), NULL));

    image_data = h_length_value(h_uint8(), h_many(h_uint8())); // Simplified

    extension = h_sequence(
        h_uint8(), // extension introducer
        h_uint8(), // label (graphic control, comment, etc.)
        h_length_value(h_uint8(), h_many(h_uint8())), // block size and data
        NULL
    );

    trailer = h_uint8(); // trailer

    // Complete GIF file
    gif_file = h_sequence(
        header,
        logical_screen_descriptor,
        h_optional(global_color_table),
        h_many(h_choice(image_descriptor, extension, NULL)), // multiple image or extension blocks
        trailer,
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
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET); // rewind to the beginning

    uint8_t *buf = malloc(fsize);
    fread(buf, 1, fsize, fp);
    fclose(fp);

    init_parsers();

    HParseResult *result = h_parse(gif_file, buf, fsize);
    if (result) {
        printf("Parse successful!\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        printf("Parse failed!\n");
    }

    free(buf);
    h_parse_result_free(result);
    return 0;
}