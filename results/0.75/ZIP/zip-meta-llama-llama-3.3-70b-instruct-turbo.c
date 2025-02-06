#include <hammer/hammer.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define FIXED_SIZE 4

typedef struct {
    uint32_t local_header_signature;
    uint16_t version_needed_to_extract;
    uint16_t general_purpose_bit_flag;
    uint16_t compression_method;
    uint16_t last_mod_time;
    uint16_t last_mod_date;
    uint32_t crc32;
    uint32_t compressed_size;
    uint32_t uncompressed_size;
    uint16_t filename_length;
    uint16_t extra_field_length;
} local_file_header;

typedef struct {
    uint32_t crc32;
    uint32_t compressed_size;
    uint32_t uncompressed_size;
} data_descriptor;

typedef struct {
    uint32_t central_header_signature;
    uint16_t version_made_by;
    uint16_t version_needed_to_extract;
    uint16_t general_purpose_bit_flag;
    uint16_t compression_method;
    uint16_t last_mod_time;
    uint16_t last_mod_date;
    uint32_t crc32;
    uint32_t compressed_size;
    uint32_t uncompressed_size;
    uint16_t filename_length;
    uint16_t extra_field_length;
    uint16_t file_comment_length;
    uint16_t disk_number_start;
    uint16_t int_attributes;
    uint32_t ext_attributes;
    uint32_t local_header_offset;
} central_directory_header;

typedef struct {
    uint32_t end_of_central_dir_signature;
    uint16_t number_of_this_disk;
    uint16_t number_of_disk_with_start_of_central_dir;
    uint16_t total_number_of_entries_in_central_dir_on_this_disk;
    uint16_t total_number_of_entries_in_central_dir;
    uint32_t size_of_central_dir;
    uint32_t offset_of_start_of_central_dir;
    uint16_t zipfile_comment_length;
} end_of_central_directory;

void* local_file_header_rule(void* input) {
    local_file_header* header = malloc(sizeof(local_file_header));
    header->local_header_signature = *(uint32_t*)input;
    input += sizeof(uint32_t);
    header->version_needed_to_extract = *(uint16_t*)input;
    input += sizeof(uint16_t);
    header->general_purpose_bit_flag = *(uint16_t*)input;
    input += sizeof(uint16_t);
    header->compression_method = *(uint16_t*)input;
    input += sizeof(uint16_t);
    header->last_mod_time = *(uint16_t*)input;
    input += sizeof(uint16_t);
    header->last_mod_date = *(uint16_t*)input;
    input += sizeof(uint16_t);
    header->crc32 = *(uint32_t*)input;
    input += sizeof(uint32_t);
    header->compressed_size = *(uint32_t*)input;
    input += sizeof(uint32_t);
    header->uncompressed_size = *(uint32_t*)input;
    input += sizeof(uint32_t);
    header->filename_length = *(uint16_t*)input;
    input += sizeof(uint16_t);
    header->extra_field_length = *(uint16_t*)input;
    input += sizeof(uint16_t);
    return header;
}

void* data_descriptor_rule(void* input) {
    data_descriptor* descriptor = malloc(sizeof(data_descriptor));
    descriptor->crc32 = *(uint32_t*)input;
    input += sizeof(uint32_t);
    descriptor->compressed_size = *(uint32_t*)input;
    input += sizeof(uint32_t);
    descriptor->uncompressed_size = *(uint32_t*)input;
    input += sizeof(uint32_t);
    return descriptor;
}

void* central_directory_header_rule(void* input) {
    central_directory_header* header = malloc(sizeof(central_directory_header));
    header->central_header_signature = *(uint32_t*)input;
    input += sizeof(uint32_t);
    header->version_made_by = *(uint16_t*)input;
    input += sizeof(uint16_t);
    header->version_needed_to_extract = *(uint16_t*)input;
    input += sizeof(uint16_t);
    header->general_purpose_bit_flag = *(uint16_t*)input;
    input += sizeof(uint16_t);
    header->compression_method = *(uint16_t*)input;
    input += sizeof(uint16_t);
    header->last_mod_time = *(uint16_t*)input;
    input += sizeof(uint16_t);
    header->last_mod_date = *(uint16_t*)input;
    input += sizeof(uint16_t);
    header->crc32 = *(uint32_t*)input;
    input += sizeof(uint32_t);
    header->compressed_size = *(uint32_t*)input;
    input += sizeof(uint32_t);
    header->uncompressed_size = *(uint32_t*)input;
    input += sizeof(uint32_t);
    header->filename_length = *(uint16_t*)input;
    input += sizeof(uint16_t);
    header->extra_field_length = *(uint16_t*)input;
    input += sizeof(uint16_t);
    header->file_comment_length = *(uint16_t*)input;
    input += sizeof(uint16_t);
    header->disk_number_start = *(uint16_t*)input;
    input += sizeof(uint16_t);
    header->int_attributes = *(uint16_t*)input;
    input += sizeof(uint16_t);
    header->ext_attributes = *(uint32_t*)input;
    input += sizeof(uint32_t);
    header->local_header_offset = *(uint32_t*)input;
    input += sizeof(uint32_t);
    return header;
}

void* end_of_central_directory_rule(void* input) {
    end_of_central_directory* directory = malloc(sizeof(end_of_central_directory));
    directory->end_of_central_dir_signature = *(uint32_t*)input;
    input += sizeof(uint32_t);
    directory->number_of_this_disk = *(uint16_t*)input;
    input += sizeof(uint16_t);
    directory->number_of_disk_with_start_of_central_dir = *(uint16_t*)input;
    input += sizeof(uint16_t);
    directory->total_number_of_entries_in_central_dir_on_this_disk = *(uint16_t*)input;
    input += sizeof(uint16_t);
    directory->total_number_of_entries_in_central_dir = *(uint16_t*)input;
    input += sizeof(uint16_t);
    directory->size_of_central_dir = *(uint32_t*)input;
    input += sizeof(uint32_t);
    directory->offset_of_start_of_central_dir = *(uint32_t*)input;
    input += sizeof(uint32_t);
    directory->zipfile_comment_length = *(uint16_t*)input;
    input += sizeof(uint16_t);
    return directory;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file: %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* data = malloc(file_size);
    fread(data, 1, file_size, file);

    local_file_header* local_file_headers[1024];
    int local_file_header_count = 0;

    uint8_t* input = data;
    while (input < data + file_size) {
        local_file_header* header = local_file_header_rule(input);
        if (header->local_header_signature != 0x04034b50) {
            break;
        }

        local_file_headers[local_file_header_count] = header;
        local_file_header_count++;

        input += sizeof(uint32_t) + sizeof(uint16_t) + sizeof(uint16_t) + sizeof(uint16_t) + sizeof(uint16_t) + sizeof(uint16_t) + sizeof(uint32_t) + sizeof(uint32_t) + sizeof(uint32_t) + sizeof(uint16_t) + sizeof(uint16_t);
        input += header->filename_length + header->extra_field_length;

        data_descriptor* descriptor = data_descriptor_rule(input);
        input += sizeof(uint32_t) + sizeof(uint32_t) + sizeof(uint32_t);
    }

    central_directory_header* central_directory_headers[1024];
    int central_directory_header_count = 0;

    while (input < data + file_size) {
        central_directory_header* header = central_directory_header_rule(input);
        if (header->central_header_signature != 0x02014b50) {
            break;
        }

        central_directory_headers[central_directory_header_count] = header;
        central_directory_header_count++;

        input += sizeof(uint32_t) + sizeof(uint16_t) + sizeof(uint16_t) + sizeof(uint16_t) + sizeof(uint16_t) + sizeof(uint16_t) + sizeof(uint16_t) + sizeof(uint32_t) + sizeof(uint32_t) + sizeof(uint32_t) + sizeof(uint16_t) + sizeof(uint16_t) + sizeof(uint16_t) + sizeof(uint16_t) + sizeof(uint16_t) + sizeof(uint32_t) + sizeof(uint32_t);
        input += header->filename_length + header->extra_field_length + header->file_comment_length;
    }

    end_of_central_directory* end_of_central_directory = end_of_central_directory_rule(input);

    fclose(file);
    free(data);
    return 0;
}