#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

static HParser* gif_header;
static HParser* logical_screen_descriptor;
static HParser* global_color_table;
static HParser* graphic_control_extension;
static HParser* image_descriptor;
static HParser* local_color_table;
static HParser* image_data;
static HParser* plain_text_extension;
static HParser* application_extension;
static HParser* comment_extension;
static HParser* gif_trailer;
static HParser* gif_data_block;
static HParser* gif_data_blocks;
static HParser* gif_block;
static HParser* gif_file;

void init_parsers(void) {
    // GIF Header
    gif_header = h_sequence(
        h_token((const uint8_t*)"GIF", 3),
        h_token((const uint8_t*)"87a", 3),
        NULL
    );

    // Logical Screen Descriptor
    logical_screen_descriptor = h_sequence(
        h_uint16(), // width
        h_uint16(), // height
        h_bits(8, false), // packed field
        h_uint8(), // background color index
        h_uint8(), // pixel aspect ratio
        NULL
    );

    // Global Color Table
    global_color_table = h_many(h_uint8());

    // Graphic Control Extension
    graphic_control_extension = h_sequence(
        h_ch(0x21), // extension introducer
        h_ch(0xF9), // graphic control label
        h_ch(0x04), // block size
        h_bits(8, false), // packed field
        h_uint16(), // delay time
        h_uint8(), // transparent color index
        h_ch(0x00), // block terminator
        NULL
    );

    // Image Descriptor
    image_descriptor = h_sequence(
        h_ch(0x2C), // image separator
        h_uint16(), // left position
        h_uint16(), // top position
        h_uint16(), // width
        h_uint16(), // height
        h_bits(8, false), // packed field
        NULL
    );

    // Local Color Table
    local_color_table = h_many(h_uint8());

    // Image Data
    gif_data_block = h_sequence(
        h_uint8(), // block size
        h_many(h_uint8()), // data sub-blocks
        NULL
    );
    
    gif_data_blocks = h_many(gif_data_block);

    image_data = h_sequence(
        h_uint8(), // LZW minimum code size
        gif_data_blocks,
        h_ch(0x00), // block terminator
        NULL
    );

    // Plain Text Extension
    plain_text_extension = h_sequence(
        h_ch(0x21), // extension introducer
        h_ch(0x01), // plain text label
        h_uint8(), // block size
        h_many(h_uint8()), // text data
        h_ch(0x00), // block terminator
        NULL
    );

    // Application Extension
    application_extension = h_sequence(
        h_ch(0x21), // extension introducer
        h_ch(0xFF), // application extension label
        h_ch(0x0B), // block size
        h_many(h_uint8()), // application identifier and authentication code
        gif_data_blocks,
        h_ch(0x00), // block terminator
        NULL
    );

    // Comment Extension
    comment_extension = h_sequence(
        h_ch(0x21), // extension introducer
        h_ch(0xFE), // comment label
        gif_data_blocks,
        h_ch(0x00), // block terminator
        NULL
    );

    // GIF Trailer
    gif_trailer = h_ch(0x3B);

    // GIF Block
    gif_block = h_choice(
        graphic_control_extension,
        image_descriptor,
        plain_text_extension,
        application_extension,
        comment_extension,
        NULL
    );

    // Complete GIF File
    gif_file = h_sequence(
        gif_header,
        logical_screen_descriptor,
        h_optional(global_color_table),
        h_many(gif_block),
        gif_trailer,
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
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *input = malloc(file_size);
    if (!input) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if (fread(input, 1, file_size, file) != file_size) {
        perror("Failed to read file");
        free(input);
        fclose(file);
        return 1;
    }

    init_parsers();

    HParseResult *result = h_parse(gif_file, input, file_size);
    if (result) {
        printf("Successfully parsed GIF file\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse GIF file\n");
    }

    free(input);
    fclose(file);
    return 0;
}