#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define LOCAL_FILE_HEADER_SIGNATURE 0x04034b50
#define CENTRAL_DIRECTORY_HEADER_SIGNATURE 0x02014b50
#define END_OF_CENTRAL_DIRECTORY_SIGNATURE 0x06054b50

typedef struct {
    uint32_t signature;
    uint16_t version_needed_to_extract;
    uint16_t general_purpose_bit_flag;
    uint16_t compression_method;
    uint16_t last_modified_time;
    uint16_t last_modified_date;
    uint32_t crc_32;
    uint32_t compressed_size;
    uint32_t uncompressed_size;
    uint16_t file_name_length;
    uint16_t extra_field_length;
} local_file_header_t;

typedef struct {
    uint32_t signature;
    uint16_t version_made_by;
    uint16_t version_needed_to_extract;
    uint16_t general_purpose_bit_flag;
    uint16_t compression_method;
    uint16_t last_modified_time;
    uint16_t last_modified_date;
    uint32_t crc_32;
    uint32_t compressed_size;
    uint32_t uncompressed_size;
    uint16_t file_name_length;
    uint16_t extra_field_length;
    uint16_t file_comment_length;
    uint16_t disk_number_start;
    uint16_t internal_file_attributes;
    uint32_t external_file_attributes;
} central_directory_header_t;

typedef struct {
    uint32_t signature;
    uint16_t number_of_this_disk;
    uint16_t number_of_disk_where_central_directory_starts;
    uint16_t number_of_entries_in_central_directory_on_this_disk;
    uint16_t total_number_of_entries_in_central_directory;
    uint32_t size_of_central_directory;
    uint32_t offset_of_central_directory;
    uint16_t comment_length;
} end_of_central_directory_t;

void* local_file_header(void) {
    void* def = hammer_new();
    def = hammer_bind_uint32_le(def, LOCAL_FILE_HEADER_SIGNATURE);
    def = hammer_bind_uint16_le(def);
    def = hammer_bind_uint16_le(def);
    def = hammer_bind_uint16_le(def);
    def = hammer_bind_uint16_le(def);
    def = hammer_bind_uint16_le(def);
    def = hammer_bind_uint32_le(def);
    def = hammer_bind_uint32_le(def);
    def = hammer_bind_uint32_le(def);
    def = hammer_bind_uint16_le(def);
    def = hammer_bind_uint16_le(def);
    return def;
}

void* central_directory_header(void) {
    void* def = hammer_new();
    def = hammer_bind_uint32_le(def, CENTRAL_DIRECTORY_HEADER_SIGNATURE);
    def = hammer_bind_uint16_le(def);
    def = hammer_bind_uint16_le(def);
    def = hammer_bind_uint16_le(def);
    def = hammer_bind_uint16_le(def);
    def = hammer_bind_uint16_le(def);
    def = hammer_bind_uint16_le(def);
    def = hammer_bind_uint32_le(def);
    def = hammer_bind_uint32_le(def);
    def = hammer_bind_uint32_le(def);
    def = hammer_bind_uint16_le(def);
    def = hammer_bind_uint16_le(def);
    def = hammer_bind_uint16_le(def);
    def = hammer_bind_uint16_le(def);
    def = hammer_bind_uint16_le(def);
    def = hammer_bind_uint16_le(def);
    def = hammer_bind_uint32_le(def);
    return def;
}

void* end_of_central_directory(void) {
    void* def = hammer_new();
    def = hammer_bind_uint32_le(def, END_OF_CENTRAL_DIRECTORY_SIGNATURE);
    def = hammer_bind_uint16_le(def);
    def = hammer_bind_uint16_le(def);
    def = hammer_bind_uint16_le(def);
    def = hammer_bind_uint16_le(def);
    def = hammer_bind_uint32_le(def);
    def = hammer_bind_uint32_le(def);
    def = hammer_bind_uint16_le(def);
    return def;
}

void* zip_file(void) {
    void* def = hammer_new();
    def = hammer_repeat(def, local_file_header());
    def = hammer_bind(def, central_directory_header());
    def = hammer_bind_bytes_le(def, hammer_bind_uint16_le(hammer_new()));
    def = hammer_bind_bytes_le(def, hammer_bind_uint16_le(hammer_new()));
    def = hammer_bind_bytes_le(def, hammer_bind_uint16_le(hammer_new()));
    def = hammer_bind(def, end_of_central_directory());
    def = hammer_bind_bytes_le(def, hammer_bind_uint16_le(hammer_new()));
    return def;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        printf("Usage: %s <zip_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t *data = malloc(file_size);
    if (!data) {
        printf("Error allocating memory\n");
        return 1;
    }

    if (fread(data, 1, file_size, file) != file_size) {
        printf("Error reading file\n");
        return 1;
    }

    fclose(file);

    void* parser = hammer_init(zip_file(), data, file_size);
    if (!hammer_parse(&parser)) {
        printf("Error parsing ZIP file\n");
        return 1;
    }

    free(data);
    return 0;
}