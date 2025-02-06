#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <hammer/hammer.h>

typedef struct {
    uint8_t id1;
    uint8_t id2;
    uint8_t compression_method;
    struct {
        uint8_t text_hint : 1;
        uint8_t header_crc : 1;
        uint8_t extra_field : 1;
        uint8_t orig_filename : 1;
        uint8_t file_comment : 1;
        uint8_t reserved : 3;
    } flags;
    uint32_t modification_time;
    uint8_t extra_flags;
    uint8_t operating_system;
    
    struct {
        bool present;
        uint8_t* data;
        size_t length;
    } extra_field;
    
    struct {
        bool present;
        char* filename;
    } orig_filename;
    
    struct {
        bool present;
        char* comment;
    } file_comment;
    
    uint16_t header_crc;
    
    uint8_t* compressed_data;
    size_t compressed_length;
    
    uint32_t data_crc32;
    uint32_t original_length;
} GzipFile;

HParser* parse_gzip_header() {
    HParser* id1 = h_literal(h_ch(0x1F));
    HParser* id2 = h_literal(h_ch(0x8B));
    HParser* compression_method = h_literal(h_ch(0x08));
    
    HParser* flags = h_bits(8, false);
    HParser* modification_time = h_uint32();
    HParser* extra_flags = h_uint8();
    HParser* operating_system = h_uint8();
    
    HParser* gzip_header = h_sequence(
        id1, 
        id2, 
        compression_method, 
        flags,
        modification_time,
        extra_flags,
        operating_system,
        NULL
    );
    
    return gzip_header;
}

HParser* parse_optional_fields() {
    HParser* extra_field_length = h_uint16();
    HParser* extra_field_data = h_repeat_n(h_uint8(), 1);
    
    HParser* filename = h_many1(h_except(h_ch('\0')));
    HParser* comment = h_many1(h_except(h_ch('\0')));
    
    HParser* header_crc = h_uint16();
    
    HParser* optional_fields = h_choice(
        h_optional(extra_field_length),
        h_optional(extra_field_data),
        h_optional(filename),
        h_optional(comment),
        h_optional(header_crc),
        NULL
    );
    
    return optional_fields;
}

HParser* parse_gzip_file() {
    HParser* header = parse_gzip_header();
    HParser* optional = parse_optional_fields();
    HParser* compressed_data = h_many(h_uint8());
    HParser* data_crc32 = h_uint32();
    HParser* original_length = h_uint32();
    
    HParser* gzip_file = h_sequence(
        header,
        optional,
        compressed_data,
        data_crc32,
        original_length,
        NULL
    );
    
    return gzip_file;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gzip_file>\n", argv[0]);
        return 1;
    }
    
    FILE *file = fopen(argv[1], "rb");
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
    
    HParser* gzip_parser = parse_gzip_file();
    HParseResult* result = h_parse(gzip_parser, buffer, file_size);
    
    if (result && result->ast) {
        printf("GZIP file parsed successfully\n");
    } else {
        printf("GZIP file parsing failed\n");
    }
    
    free(buffer);
    fclose(file);
    h_parse_result_free(result);
    h_arena_free(h_get_allocator());
    
    return 0;
}