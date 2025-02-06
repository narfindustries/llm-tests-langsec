#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef enum {
    NO_COMPRESSION = 0,
    SHRINK = 1,
    REDUCE_1 = 2,
    REDUCE_2 = 3,
    REDUCE_3 = 4,
    REDUCE_4 = 5,
    IMPLODE = 6,
    DEFLATE = 8,
    DEFLATE64 = 9,
    BZIP2 = 12,
    LZMA = 14,
    IBM_TERSE = 18,
    LZ77 = 19,
    PPMD = 98
} CompressionMethod;

typedef struct {
    uint32_t signature;
    uint16_t version_needed;
    uint16_t general_purpose_flag;
    CompressionMethod compression_method;
    uint16_t last_mod_time;
    uint16_t last_mod_date;
    uint32_t crc32;
    uint32_t compressed_size;
    uint32_t uncompressed_size;
    uint16_t filename_length;
    uint16_t extra_field_length;
    char* filename;
    uint8_t* extra_field;
} LocalFileHeader;

HParsedToken* parse_local_file_header(const HParseResult* result, void* user_data) {
    LocalFileHeader* header = malloc(sizeof(LocalFileHeader));
    const HParsedToken* seq = result->ast;

    header->signature = *(uint32_t*)seq->seq[0]->data;
    header->version_needed = *(uint16_t*)seq->seq[1]->data;
    header->general_purpose_flag = *(uint16_t*)seq->seq[2]->data;
    header->compression_method = *(uint16_t*)seq->seq[3]->data;
    header->last_mod_time = *(uint16_t*)seq->seq[4]->data;
    header->last_mod_date = *(uint16_t*)seq->seq[5]->data;
    header->crc32 = *(uint32_t*)seq->seq[6]->data;
    header->compressed_size = *(uint32_t*)seq->seq[7]->data;
    header->uncompressed_size = *(uint32_t*)seq->seq[8]->data;
    header->filename_length = *(uint16_t*)seq->seq[9]->data;
    header->extra_field_length = *(uint16_t*)seq->seq[10]->data;
    
    header->filename = malloc(header->filename_length + 1);
    memcpy(header->filename, seq->seq[11]->data, header->filename_length);
    header->filename[header->filename_length] = '\0';

    header->extra_field = malloc(header->extra_field_length);
    memcpy(header->extra_field, seq->seq[12]->data, header->extra_field_length);

    return h_make_seq(header);
}

HParser* create_zip_parser() {
    HParser* signature = h_uint32();
    HParser* version_needed = h_uint16();
    HParser* general_purpose_flag = h_uint16();
    HParser* compression_method = h_uint16();
    HParser* last_mod_time = h_uint16();
    HParser* last_mod_date = h_uint16();
    HParser* crc32 = h_uint32();
    HParser* compressed_size = h_uint32();
    HParser* uncompressed_size = h_uint32();
    HParser* filename_length = h_uint16();
    HParser* extra_field_length = h_uint16();
    
    HParser* filename = h_length_value(filename_length, h_many1(h_ch_range(0, 255)));
    HParser* extra_field = h_length_value(extra_field_length, h_many1(h_ch_range(0, 255)));

    HParser* local_file_header = h_sequence(
        h_literal_uint32(0x04034B50),
        version_needed,
        general_purpose_flag,
        compression_method,
        last_mod_time,
        last_mod_date,
        crc32,
        compressed_size,
        uncompressed_size,
        filename_length,
        extra_field_length,
        filename,
        extra_field,
        NULL
    );

    return h_action(local_file_header, parse_local_file_header, NULL);
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <zipfile>\n", argv[0]);
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
    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser* zip_parser = create_zip_parser();
    HParseResult* result = h_parse(zip_parser, buffer, file_size);

    if (result && result->ast) {
        LocalFileHeader* header = (LocalFileHeader*)result->ast->seq[0];
        printf("ZIP File Header Parsed Successfully\n");
        printf("Compression Method: %d\n", header->compression_method);
        printf("Filename: %s\n", header->filename);
    } else {
        printf("Parsing failed\n");
    }

    free(buffer);
    h_parse_result_free(result);
    h_destroy_parser(zip_parser);

    return 0;
}