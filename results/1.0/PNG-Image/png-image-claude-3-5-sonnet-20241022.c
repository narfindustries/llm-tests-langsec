#include <hammer/hammer.h>
#include <stdio.h>

static const uint8_t SIGNATURE[] = {0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A};

HParser* init_png_parser() {
    HParser *signature = h_token(SIGNATURE, sizeof(SIGNATURE));
    
    HParser *chunk_length = h_uint32_be();
    HParser *chunk_type = h_bytes(4);
    HParser *crc = h_uint32_be();
    
    H_RULE(chunk_data, h_many_bytes(h_left_length(h_get_value(chunk_length))));
    
    H_RULE(chunk, h_sequence(chunk_length, chunk_type, chunk_data, crc, NULL));
    H_RULE(chunks, h_many1(chunk));
    
    H_RULE(png_file, h_sequence(signature, chunks, NULL));
    
    return png_file;
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
    size_t size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* buffer = malloc(size);
    if (!buffer) {
        fclose(file);
        perror("Failed to allocate memory");
        return 1;
    }

    if (fread(buffer, 1, size, file, NULL) != size) {
        free(buffer);
        fclose(file);
        perror("Failed to read file");
        return 1;
    }
    fclose(file);

    HParser* parser = init_png_parser();
    HParseResult* result = h_parse(parser, buffer, size);

    if (!result) {
        fprintf(stderr, "Failed to parse PNG file\n");
        free(buffer);
        return 1;
    }

    h_parse_result_free(result);
    free(buffer);
    return 0;
}