#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    uint8_t id1;
    uint8_t id2;
    uint8_t cm;
    uint8_t flg;
    uint32_t mtime;
    uint8_t xfl;
    uint8_t os;
    
    struct {
        int has_extra;
        uint8_t* extra_data;
        size_t extra_len;
    } extra;
    
    struct {
        int has_name;
        char* filename;
    } fname;
    
    struct {
        int has_comment;
        char* comment;
    } fcomment;
    
    uint16_t header_crc;
    
    uint8_t* compressed_data;
    size_t compressed_len;
    
    uint32_t crc32;
    uint32_t uncompressed_size;
} GzipFile;

HParseResult* parse_gzip_header(const uint8_t* input, size_t len) {
    HParser* id1 = h_ch(0x1F);
    HParser* id2 = h_ch(0x8B);
    HParser* cm = h_ch(8);
    
    HParser* flg = h_bits(8, false);
    HParser* mtime = h_uint32();
    HParser* xfl = h_bits(8, false);
    HParser* os = h_bits(8, false);
    
    HParser* extra_len = h_uint16();
    HParser* extra_data = h_repeat_n(h_bits(8, false), 1);
    
    HParser* non_zero_char = h_in_range(1, 255);
    HParser* filename = h_many(non_zero_char);
    HParser* comment = h_many(non_zero_char);
    
    HParser* header_crc = h_uint16();
    
    HParser* gzip_parser = h_sequence(
        id1, id2, cm, flg, mtime, xfl, os,
        h_optional(extra_data),
        h_optional(filename),
        h_optional(comment),
        h_optional(header_crc),
        NULL
    );
    
    return h_parse(gzip_parser, input, len);
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gzip_file>\n", argv[0]);
        return 1;
    }
    
    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }
    
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }
    
    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("File read error");
        free(buffer);
        fclose(file);
        return 1;
    }
    
    fclose(file);
    
    HParseResult* result = parse_gzip_header(buffer, file_size);
    if (!result) {
        fprintf(stderr, "Parsing failed\n");
        free(buffer);
        return 1;
    }
    
    h_parse_result_free(result);
    free(buffer);
    
    return 0;
}