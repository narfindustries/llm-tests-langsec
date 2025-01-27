#include <hammer/hammer.h>
#include <stdio.h>

static HParser* init_png_parser() {
    // PNG signature
    HParser* signature = h_token((uint8_t*)"\x89PNG\r\n\x1a\n", 8);
    
    // Length field (4 bytes)
    HParser* length = h_uint32_be();
    
    // Chunk type (4 bytes)
    HParser* chunk_type = h_token_array(4);
    
    // CRC (4 bytes)
    HParser* crc = h_uint32_be();
    
    // Chunk data (variable length based on length field)
    HParser* chunk_data = h_length_value(length, h_uint8());
    
    // Single chunk structure
    HParser* chunk = h_sequence(length, chunk_type, chunk_data, crc, NULL);
    
    // Complete PNG format: signature followed by one or more chunks
    return h_sequence(signature, h_many1(chunk), NULL);
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

    uint8_t* input = malloc(size);
    if (!input) {
        fclose(file);
        fprintf(stderr, "Memory allocation failed\n");
        return 1;
    }

    if (fread(input, 1, size, file, NULL) != size) {
        free(input);
        fclose(file);
        fprintf(stderr, "Failed to read file\n");
        return 1;
    }
    fclose(file);

    HParser* parser = init_png_parser();
    HParseResult* result = h_parse(parser, input, size);

    if (!result) {
        fprintf(stderr, "Parse failed\n");
        free(input);
        return 1;
    }

    h_parse_result_free(result);
    free(input);
    return 0;
}