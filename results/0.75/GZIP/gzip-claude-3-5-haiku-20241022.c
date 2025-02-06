#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <hammer/hammer.h>

typedef struct {
    uint8_t magic_1;
    uint8_t magic_2;
    uint8_t compression_method;
    uint8_t flags;
    uint32_t modification_time;
    uint8_t extra_flags;
    uint8_t operating_system;
    
    struct {
        bool has_extra;
        bool has_name;
        bool has_comment;
        bool has_crc;
    } flag_state;
    
    HParser* extra_fields;
    char* filename;
    char* comment;
    uint16_t header_crc;
    
    HParser* compressed_data;
    uint32_t data_crc32;
    uint32_t original_length;
} GzipFile;

HParser* gzip_parser() {
    HParser* magic = h_sequence(
        h_ch(0x1F),
        h_ch(0x8B),
        NULL
    );
    
    HParser* compression_method = h_ch(0x08);
    
    HParser* flags = h_bits(8, false);
    
    HParser* modification_time = h_uint32();
    
    HParser* extra_flags = h_bits(8, false);
    
    HParser* operating_system = h_bits(8, false);
    
    HParser* optional_extra_fields = h_optional(
        h_sequence(
            h_uint16(),
            h_repeat_n(h_bits(8, false), 1),
            NULL
        )
    );
    
    HParser* optional_filename = h_optional(
        h_token((uint8_t*)"", 0)
    );
    
    HParser* optional_comment = h_optional(
        h_token((uint8_t*)"", 0)
    );
    
    HParser* optional_header_crc = h_optional(
        h_uint16()
    );
    
    HParser* compressed_data = h_repeat_n(h_bits(8, false), 1);
    
    HParser* data_crc32 = h_uint32();
    
    HParser* original_length = h_uint32();
    
    return h_sequence(
        magic,
        compression_method,
        flags,
        modification_time,
        extra_flags,
        operating_system,
        optional_extra_fields,
        optional_filename,
        optional_comment,
        optional_header_crc,
        compressed_data,
        data_crc32,
        original_length,
        NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gzip_file>\n", argv[0]);
        return 1;
    }
    
    int fd = open(argv[1], O_RDONLY);
    if (fd == -1) {
        perror("Could not open file");
        return 1;
    }
    
    off_t file_size = lseek(fd, 0, SEEK_END);
    lseek(fd, 0, SEEK_SET);
    
    uint8_t* buffer = malloc(file_size);
    if (read(fd, buffer, file_size) != file_size) {
        perror("Could not read file");
        close(fd);
        free(buffer);
        return 1;
    }
    
    close(fd);
    
    HParser* parser = gzip_parser();
    HParseResult* result = h_parse(parser, buffer, file_size);
    
    if (result && result->ast) {
        printf("Successfully parsed GZIP file\n");
    } else {
        printf("Failed to parse GZIP file\n");
    }
    
    h_parse_result_free(result);
    h_arena_free(parser->arena);
    free(buffer);
    
    return 0;
}