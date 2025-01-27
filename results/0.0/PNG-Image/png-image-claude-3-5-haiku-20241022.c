#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// PNG Image Parsing Specification
static HParser* png_signature(void) {
    return h_literal_bytes((uint8_t*)"\x89PNG\r\n\x1a\n", 8);
}

static HParser* png_chunk_length(void) {
    return h_uint32();
}

static HParser* png_chunk_type(void) {
    return h_choice(
        h_literal_bytes((uint8_t*)"IHDR", 4),
        h_literal_bytes((uint8_t*)"IDAT", 4),
        h_literal_bytes((uint8_t*)"IEND", 4),
        NULL
    );
}

static HParser* png_chunk_data(void) {
    HParsedToken* length_token = h_parse(png_chunk_length(), NULL);
    uint32_t length = length_token ? length_token->uint32 : 0;
    return h_repeat_n(h_uint8(), length);
}

static HParser* png_chunk_crc(void) {
    return h_uint32();
}

static HParser* png_chunk(void) {
    return h_sequence(
        png_chunk_length(),
        png_chunk_type(), 
        png_chunk_data(),
        png_chunk_crc(),
        NULL
    );
}

static HParser* png_file(void) {
    return h_sequence(
        png_signature(),
        h_many(png_chunk()),
        NULL
    );
}

int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <png_file>\n", argv[0]);
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

    HParser* parser = png_file();
    HParsedToken* result = h_parse(parser, buffer, read_size);

    if (result) {
        printf("PNG file parsed successfully\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "PNG file parsing failed\n");
    }

    free(buffer);
    h_destroy_parser(parser);
    return 0;
}