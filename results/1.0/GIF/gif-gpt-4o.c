#include <hammer/hammer.h>

typedef struct {
    uint8_t signature[3];
    uint8_t version[3];
    uint16_t width;
    uint16_t height;
    uint8_t packed_fields;
    uint8_t background_color_index;
    uint8_t pixel_aspect_ratio;
    // Add additional fields if needed
} GifHeader;

typedef struct {
    // Define fields based on the GIF structure you intend to parse
} GifData;

static HParsedToken *read_uint16(HParseContext *ctx, H_ARGLIST) {
    H_DYNARG(modifiers, size_t, size);
    uint8_t *bytes = H_PARSER_EAT(ctx, size);
    return H_MAKE_UINT(((uint16_t)bytes[1] << 8) | bytes[0]);
}

HParser *create_gif_header_parser() {
    HParser *header_parser = h_sequence(
        h_ch('G'), h_ch('I'), h_ch('F'),
        h_ch_range('8', '9'), h_ch_range('a', 'z'), h_ch_range('a', 'z'),
        h_bind_uint16(), h_bind_uint16(),
        h_uint8(), h_uint8(), h_uint8(),
        NULL
    );

    return header_parser;
}

HParser *create_file_parser() {
    HParser *header_parser = create_gif_header_parser();
    return h_right(header_parser, h_end_p());
}

int main(int argc, char **argv) {
    HParser *file_parser = create_file_parser();
    
    const char *filename = "input.gif"; // adjust filename as needed
    FILE *f = fopen(filename, "rb");
    if (!f) {
        perror("Error opening file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    long length = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t *data = (uint8_t *)malloc(length);
    if (!data) {
        perror("Error allocating memory");
        fclose(f);
        return 1;
    }

    fread(data, 1, length, f);
    fclose(f);

    HParseResult *result = h_parse(file_parser, data, length);
    if (result) {
        // Successfully parsed the GIF header
        const GifHeader *header = (const GifHeader *)result->ast->token->data;
        // Do something with the header...
        
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse GIF file\n");
    }

    free(data);
    h_parser_free(file_parser);

    return 0;
}