#include <hammer/hammer.h>
#include <stdio.h>

static const uint8_t PNG_SIGNATURE[] = {0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A};

HParser* init_png_parser() {
    // PNG chunk structure parsers
    HParser* length = h_uint32_be();
    HParser* type = h_token((const uint8_t*)"IHDR", 4);
    HParser* width = h_uint32_be();
    HParser* height = h_uint32_be();
    HParser* bit_depth = h_uint8();
    HParser* color_type = h_uint8();
    HParser* compression = h_uint8();
    HParser* filter = h_uint8();
    HParser* interlace = h_uint8();
    HParser* crc = h_uint32_be();

    // IHDR chunk content
    HParser* ihdr_content = h_sequence(width, height, bit_depth, color_type, 
                                     compression, filter, interlace, NULL);

    // Complete IHDR chunk
    HParser* ihdr_chunk = h_sequence(length, type, ihdr_content, crc, NULL);

    // PNG signature
    HParser* signature = h_token(PNG_SIGNATURE, sizeof(PNG_SIGNATURE));

    // Complete PNG header (signature + IHDR chunk)
    return h_sequence(signature, ihdr_chunk, NULL);
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <png_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        fclose(file);
        fprintf(stderr, "Memory allocation failed\n");
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        free(buffer);
        fclose(file);
        fprintf(stderr, "Failed to read file\n");
        return 1;
    }
    fclose(file);

    HParser* png_parser = init_png_parser();
    HParseResult* result = h_parse(png_parser, buffer, file_size);

    if (!result) {
        fprintf(stderr, "Failed to parse PNG file\n");
        free(buffer);
        return 1;
    }

    printf("Successfully parsed PNG header\n");

    h_parse_result_free(result);
    free(buffer);
    return 0;
}