#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    uint8_t identification[2];
    uint8_t compression_method;
    uint8_t flags;
    uint32_t modification_time;
    uint8_t extra_flags;
    uint8_t operating_system;
    
    struct {
        uint8_t* data;
        size_t length;
    } extra_fields;
    
    char* original_filename;
    char* file_comment;
    
    uint16_t header_crc;
    
    struct {
        uint8_t* compressed_data;
        size_t length;
    } compressed_block;
    
    uint32_t crc32;
    uint32_t uncompressed_size;
} GzipFile;

HParseResult* parse_gzip(const uint8_t* input, size_t length) {
    HParser* identification = h_literal_bytes((const uint8_t[]){0x1F, 0x8B}, 2);
    HParser* compression_method = h_uint8();
    HParser* flags = h_uint8();
    HParser* modification_time = h_int32le();
    HParser* extra_flags = h_uint8();
    HParser* operating_system = h_uint8();
    
    HParser* extra_fields_length = h_int16le();
    HParser* extra_fields_data = h_repeat_n(h_uint8(), h_cast_u(extra_fields_length));
    
    HParser* filename = h_many(h_in_range(1, 255));
    HParser* comment = h_many(h_in_range(1, 255));
    
    HParser* header_crc = h_int16le();
    
    HParser* compressed_data_length = h_int32le();
    HParser* compressed_data = h_repeat_n(h_uint8(), h_cast_u(compressed_data_length));
    
    HParser* crc32 = h_int32le();
    HParser* uncompressed_size = h_int32le();
    
    HParser* gzip_parser = h_sequence(
        identification,
        compression_method,
        flags,
        modification_time,
        extra_flags,
        operating_system,
        h_optional(extra_fields_data),
        h_optional(filename),
        h_optional(comment),
        h_optional(header_crc),
        compressed_data,
        crc32,
        uncompressed_size,
        NULL
    );
    
    return h_parse(gzip_parser, input, length);
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
    
    HParseResult* result = parse_gzip(buffer, file_size);
    
    if (result) {
        printf("GZIP file parsed successfully\n");
        h_parse_result_free(result);
    } else {
        printf("GZIP file parsing failed\n");
    }
    
    free(buffer);
    return 0;
}