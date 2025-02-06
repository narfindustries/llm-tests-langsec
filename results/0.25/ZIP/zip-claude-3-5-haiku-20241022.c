#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct {
    uint16_t version_needed;
    uint16_t bit_flag;
    uint16_t compression_method;
    uint16_t last_mod_time;
    uint16_t last_mod_date;
    uint32_t crc32;
    uint32_t compressed_size;
    uint32_t uncompressed_size;
    char* filename;
    uint8_t* extra_field;
} LocalFileHeader;

typedef struct {
    uint16_t version_made_by;
    uint16_t version_needed;
    uint16_t bit_flag;
    uint16_t compression_method;
    uint16_t last_mod_time;
    uint16_t last_mod_date;
    uint32_t crc32;
    uint32_t compressed_size;
    uint32_t uncompressed_size;
    char* filename;
    uint8_t* extra_field;
    char* file_comment;
    uint16_t disk_start;
    uint16_t internal_attrs;
    uint32_t external_attrs;
    uint32_t local_header_offset;
} CentralDirectoryFileHeader;

typedef struct {
    uint16_t current_disk;
    uint16_t central_dir_disk;
    uint16_t entries_on_disk;
    uint16_t total_entries;
    uint32_t central_dir_size;
    uint32_t central_dir_offset;
    char* zip_comment;
} EndOfCentralDirectoryRecord;

HParseResult* parse_local_file_header(const uint8_t* p, size_t len) {
    HParser* signature = h_literal(h_uint32(), 0x04034B50);
    HParser* version_needed = h_uint16();
    HParser* bit_flag = h_uint16();
    HParser* compression_method = h_uint16();
    HParser* last_mod_time = h_uint16();
    HParser* last_mod_date = h_uint16();
    HParser* crc32 = h_uint32();
    HParser* compressed_size = h_uint32();
    HParser* uncompressed_size = h_uint32();
    HParser* filename_length = h_uint16();
    HParser* extra_field_length = h_uint16();
    
    HParser* filename = h_length_value(filename_length, h_ch_range(0, 255));
    HParser* extra_field = h_length_value(extra_field_length, h_repeat_n(h_uint8(), (size_t)extra_field_length));

    HParser* local_file_header = h_sequence(
        signature, version_needed, bit_flag, compression_method,
        last_mod_time, last_mod_date, crc32, compressed_size,
        uncompressed_size, filename_length, extra_field_length,
        filename, extra_field, NULL
    );

    return h_parse(local_file_header, p, len);
}

HParseResult* parse_central_directory_header(const uint8_t* p, size_t len) {
    HParser* signature = h_literal(h_uint32(), 0x02014B50);
    HParser* version_made_by = h_uint16();
    HParser* version_needed = h_uint16();
    HParser* bit_flag = h_uint16();
    HParser* compression_method = h_uint16();
    HParser* last_mod_time = h_uint16();
    HParser* last_mod_date = h_uint16();
    HParser* crc32 = h_uint32();
    HParser* compressed_size = h_uint32();
    HParser* uncompressed_size = h_uint32();
    HParser* filename_length = h_uint16();
    HParser* extra_field_length = h_uint16();
    HParser* file_comment_length = h_uint16();
    HParser* disk_start = h_uint16();
    HParser* internal_attrs = h_uint16();
    HParser* external_attrs = h_uint32();
    HParser* local_header_offset = h_uint32();

    HParser* filename = h_length_value(filename_length, h_ch_range(0, 255));
    HParser* extra_field = h_length_value(extra_field_length, h_repeat_n(h_uint8(), (size_t)extra_field_length));
    HParser* file_comment = h_length_value(file_comment_length, h_ch_range(0, 255));

    HParser* central_directory_header = h_sequence(
        signature, version_made_by, version_needed, bit_flag,
        compression_method, last_mod_time, last_mod_date, crc32,
        compressed_size, uncompressed_size, filename_length,
        extra_field_length, file_comment_length, disk_start,
        internal_attrs, external_attrs, local_header_offset,
        filename, extra_field, file_comment, NULL
    );

    return h_parse(central_directory_header, p, len);
}

HParseResult* parse_end_of_central_directory(const uint8_t* p, size_t len) {
    HParser* signature = h_literal(h_uint32(), 0x06054B50);
    HParser* current_disk = h_uint16();
    HParser* central_dir_disk = h_uint16();
    HParser* entries_on_disk = h_uint16();
    HParser* total_entries = h_uint16();
    HParser* central_dir_size = h_uint32();
    HParser* central_dir_offset = h_uint32();
    HParser* zip_comment_length = h_uint16();

    HParser* zip_comment = h_length_value(zip_comment_length, h_ch_range(0, 255));

    HParser* end_of_central_directory = h_sequence(
        signature, current_disk, central_dir_disk,
        entries_on_disk, total_entries, central_dir_size,
        central_dir_offset, zip_comment_length, zip_comment, NULL
    );

    return h_parse(end_of_central_directory, p, len);
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <zip_file>\n", argv[0]);
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

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("Error reading file");
        free(buffer);
        fclose(file);
        return 1;
    }

    HParseResult* local_header = parse_local_file_header(buffer, file_size);
    HParseResult* central_header = parse_central_directory_header(buffer, file_size);
    HParseResult* end_of_central_dir = parse_end_of_central_directory(buffer, file_size);

    if (local_header && central_header && end_of_central_dir) {
        printf("ZIP file parsed successfully\n");
    } else {
        printf("ZIP file parsing failed\n");
    }

    free(buffer);
    fclose(file);
    return 0;
}