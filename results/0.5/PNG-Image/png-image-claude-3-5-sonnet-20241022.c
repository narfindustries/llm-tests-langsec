#include <hammer/hammer.h>
#include <stdio.h>

static HParser* init_png_parser(void) {
    // PNG signature
    H_RULE(signature, h_token((uint8_t*)"\x89PNG\r\n\x1a\n", 8));
    
    // Length (4 bytes)
    H_RULE(length, h_uint32_be());
    
    // Chunk type (4 bytes)
    H_RULE(chunk_type, h_token_array(4));
    
    // CRC (4 bytes)
    H_RULE(crc, h_uint32_be());
    
    // Chunk data (variable length based on length field)
    H_RULE(chunk_data, h_length_value(h_uint32_be(), h_uint8()));
    
    // Single chunk structure
    H_RULE(chunk, h_sequence(length, chunk_type, chunk_data, crc, NULL));
    
    // Complete PNG format
    H_RULE(png, h_sequence(signature, h_many1(chunk), NULL));
    
    return png;
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
    
    uint8_t* input = malloc(file_size);
    if (!input) {
        fclose(file);
        fprintf(stderr, "Memory allocation failed\n");
        return 1;
    }
    
    if (fread(input, 1, file_size, file) != file_size) {
        free(input);
        fclose(file);
        fprintf(stderr, "Failed to read file\n");
        return 1;
    }
    
    fclose(file);
    
    HParser* parser = init_png_parser();
    if (!parser) {
        free(input);
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }
    
    HParseResult* result = h_parse(parser, input, file_size);
    if (!result) {
        free(input);
        fprintf(stderr, "Parse failed\n");
        return 1;
    }
    
    printf("PNG file parsed successfully\n");
    
    h_parse_result_free(result);
    free(input);
    return 0;
}