#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <time.h>

typedef struct {
    uint8_t magic_id1;
    uint8_t magic_id2;
    uint8_t compression_method;
    struct {
        bool ftext;
        bool fhcrc;
        bool fextra;
        bool fname;
        bool fcomment;
        bool fencrypt;
    } flags;
    uint32_t modification_time;
    uint8_t extra_flags;
    uint8_t operating_system;
    
    struct {
        bool present;
        uint16_t extra_length;
        uint8_t* extra_data;
    } extra;
    
    struct {
        bool present;
        char* filename;
    } original_filename;
    
    struct {
        bool present;
        char* comment;
    } file_comment;
    
    uint16_t header_crc16;
} GzipHeader;

HParser* parse_gzip_header() {
    HParser* magic_id = h_sequence(h_ch(0x1F), h_ch(0x8B), NULL);
    
    HParser* compression_method = h_ch(0x08);
    
    HParser* flags = h_bits(6, false);
    
    HParser* mod_time = h_uint32();
    
    HParser* extra_flags = h_uint8();
    
    HParser* os_type = h_uint8();
    
    HParser* extra_length = h_uint16();
    HParser* extra_data = h_optional(h_sequence(extra_length, h_repeat_n(h_uint8(), h_uint16()), NULL));
    
    HParser* filename = h_optional(h_many(h_ch_range('!', '~')));
    
    HParser* comment = h_optional(h_many(h_ch_range('!', '~')));
    
    HParser* header_crc = h_optional(h_uint16());
    
    HParser* gzip_header = h_sequence(
        magic_id,
        compression_method,
        flags,
        mod_time,
        extra_flags,
        os_type,
        extra_data,
        filename,
        comment,
        header_crc,
        NULL
    );
    
    return gzip_header;
}

int main(int argc, char* argv[]) {
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
    
    HParser* parser = parse_gzip_header();
    HParseResult* result = h_parse(parser, buffer, file_size);
    
    if (!result || !result->ast) {
        fprintf(stderr, "Parsing failed\n");
        free(buffer);
        h_parse_result_free(result);
        return 1;
    }
    
    // Process parsed data here
    
    h_parse_result_free(result);
    free(buffer);
    return 0;
}