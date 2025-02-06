#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

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
    uint16_t internal_attributes;
    uint32_t external_attributes;
} central_directory_header_t;

typedef struct {
    uint32_t signature;
    uint16_t number_of_this_disk;
    uint16_t number_of_the_disk_where_the_central_directory_starts;
    uint16_t number_of_entries_in_the_central_directory_on_this_disk;
    uint16_t total_number_of_entries_in_the_central_directory;
    uint32_t size_of_the_central_directory;
    uint32_t offset_of_the_start_of_the_central_directory;
    uint16_t comment_length;
} end_of_central_directory_t;

typedef struct {
    uint16_t header_id;
    uint16_t data_size;
    uint8_t* data;
} extra_field_t;

typedef struct {
    local_file_header_t* local_file_header;
    uint8_t* file_name;
    extra_field_t* extra_field;
    uint8_t* file_data;
} local_file_t;

typedef struct {
    central_directory_header_t* central_directory_header;
    uint8_t* file_name;
    extra_field_t* extra_field;
    uint8_t* file_comment;
} central_directory_t;

typedef struct {
    end_of_central_directory_t* end_of_central_directory;
    uint8_t* comment;
} end_of_central_directory_record_t;

void local_file_header_parser(void* parser, local_file_header_t* local_file_header) {
    H_SEQUENCE(
        H_BIND_UINT32_LE(H_CHECK(LOCAL_FILE_HEADER_SIGNATURE), local_file_header->signature),
        H_BIND_UINT16_LE(H_CHECK(0x10), local_file_header->version_needed_to_extract),
        H_BIND_UINT16_LE(H_CHECK(0x0000), local_file_header->general_purpose_bit_flag),
        H_BIND_UINT16_LE(H_CHECK(0x0000), local_file_header->compression_method),
        H_BIND_UINT16_LE(H_CHECK(0x0000), local_file_header->last_modified_time),
        H_BIND_UINT16_LE(H_CHECK(0x0000), local_file_header->last_modified_date),
        H_BIND_UINT32_LE(H_CHECK(0x00000000), local_file_header->crc_32),
        H_BIND_UINT32_LE(H_CHECK(0x00000000), local_file_header->compressed_size),
        H_BIND_UINT32_LE(H_CHECK(0x00000000), local_file_header->uncompressed_size),
        H_BIND_UINT16_LE(H_CHECK(0x0000), local_file_header->file_name_length),
        H_BIND_UINT16_LE(H_CHECK(0x0000), local_file_header->extra_field_length)
    );
}

void extra_field_parser(void* parser, extra_field_t* extra_field) {
    H_SEQUENCE(
        H_BIND_UINT16_LE(H_CHECK(0x0001), extra_field->header_id),
        H_BIND_UINT16_LE(H_CHECK(0x0000), extra_field->data_size),
        H_BIND_BYTES(extra_field->data_size, extra_field->data)
    );
}

void local_file_parser(void* parser, local_file_t* local_file) {
    H_SEQUENCE(
        H_BIND_STRUCT(local_file_header_parser, local_file->local_file_header),
        H_BIND_BYTES(local_file->local_file_header->file_name_length, local_file->file_name),
        H_BIND_STRUCT(extra_field_parser, local_file->extra_field),
        H_BIND_BYTES(local_file->local_file_header->compressed_size, local_file->file_data)
    );
}

void central_directory_header_parser(void* parser, central_directory_header_t* central_directory_header) {
    H_SEQUENCE(
        H_BIND_UINT32_LE(H_CHECK(CENTRAL_DIRECTORY_HEADER_SIGNATURE), central_directory_header->signature),
        H_BIND_UINT16_LE(H_CHECK(0x10), central_directory_header->version_made_by),
        H_BIND_UINT16_LE(H_CHECK(0x10), central_directory_header->version_needed_to_extract),
        H_BIND_UINT16_LE(H_CHECK(0x0000), central_directory_header->general_purpose_bit_flag),
        H_BIND_UINT16_LE(H_CHECK(0x0000), central_directory_header->compression_method),
        H_BIND_UINT16_LE(H_CHECK(0x0000), central_directory_header->last_modified_time),
        H_BIND_UINT16_LE(H_CHECK(0x0000), central_directory_header->last_modified_date),
        H_BIND_UINT32_LE(H_CHECK(0x00000000), central_directory_header->crc_32),
        H_BIND_UINT32_LE(H_CHECK(0x00000000), central_directory_header->compressed_size),
        H_BIND_UINT32_LE(H_CHECK(0x00000000), central_directory_header->uncompressed_size),
        H_BIND_UINT16_LE(H_CHECK(0x0000), central_directory_header->file_name_length),
        H_BIND_UINT16_LE(H_CHECK(0x0000), central_directory_header->extra_field_length),
        H_BIND_UINT16_LE(H_CHECK(0x0000), central_directory_header->file_comment_length),
        H_BIND_UINT16_LE(H_CHECK(0x0000), central_directory_header->disk_number_start),
        H_BIND_UINT16_LE(H_CHECK(0x0000), central_directory_header->internal_attributes),
        H_BIND_UINT32_LE(H_CHECK(0x00000000), central_directory_header->external_attributes)
    );
}

void central_directory_parser(void* parser, central_directory_t* central_directory) {
    H_SEQUENCE(
        H_BIND_STRUCT(central_directory_header_parser, central_directory->central_directory_header),
        H_BIND_BYTES(central_directory->central_directory_header->file_name_length, central_directory->file_name),
        H_BIND_STRUCT(extra_field_parser, central_directory->extra_field),
        H_BIND_BYTES(central_directory->central_directory_header->file_comment_length, central_directory->file_comment)
    );
}

void end_of_central_directory_parser(void* parser, end_of_central_directory_record_t* end_of_central_directory_record) {
    H_SEQUENCE(
        H_BIND_UINT32_LE(H_CHECK(END_OF_CENTRAL_DIRECTORY_SIGNATURE), end_of_central_directory_record->end_of_central_directory->signature),
        H_BIND_UINT16_LE(H_CHECK(0x0000), end_of_central_directory_record->end_of_central_directory->number_of_this_disk),
        H_BIND_UINT16_LE(H_CHECK(0x0000), end_of_central_directory_record->end_of_central_directory->number_of_the_disk_where_the_central_directory_starts),
        H_BIND_UINT16_LE(H_CHECK(0x0000), end_of_central_directory_record->end_of_central_directory->number_of_entries_in_the_central_directory_on_this_disk),
        H_BIND_UINT16_LE(H_CHECK(0x0000), end_of_central_directory_record->end_of_central_directory->total_number_of_entries_in_the_central_directory),
        H_BIND_UINT32_LE(H_CHECK(0x00000000), end_of_central_directory_record->end_of_central_directory->size_of_the_central_directory),
        H_BIND_UINT32_LE(H_CHECK(0x00000000), end_of_central_directory_record->end_of_central_directory->offset_of_the_start_of_the_central_directory),
        H_BIND_UINT16_LE(H_CHECK(0x0000), end_of_central_directory_record->end_of_central_directory->comment_length),
        H_BIND_BYTES(end_of_central_directory_record->end_of_central_directory->comment_length, end_of_central_directory_record->comment)
    );
}

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <zip_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* data = malloc(file_size);
    if (!data) {
        printf("Error allocating memory\n");
        fclose(file);
        return 1;
    }

    size_t bytes_read = fread(data, 1, file_size, file);
    if (bytes_read != file_size) {
        printf("Error reading file\n");
        free(data);
        fclose(file);
        return 1;
    }

    fclose(file);

    local_file_t local_file;
    central_directory_t central_directory;
    end_of_central_directory_record_t end_of_central_directory_record;

    int result = H_PARSE(local_file_parser, data, file_size, &local_file);
    if (result == 0) {
        printf("Local file header:\n");
        printf("  Signature: 0x%08x\n", local_file.local_file_header->signature);
        printf("  Version needed to extract: 0x%04x\n", local_file.local_file_header->version_needed_to_extract);
        printf("  General purpose bit flag: 0x%04x\n", local_file.local_file_header->general_purpose_bit_flag);
        printf("  Compression method: 0x%04x\n", local_file.local_file_header->compression_method);
        printf("  Last modified time: 0x%04x\n", local_file.local_file_header->last_modified_time);
        printf("  Last modified date: 0x%04x\n", local_file.local_file_header->last_modified_date);
        printf("  CRC-32: 0x%08x\n", local_file.local_file_header->crc_32);
        printf("  Compressed size: 0x%08x\n", local_file.local_file_header->compressed_size);
        printf("  Uncompressed size: 0x%08x\n", local_file.local_file_header->uncompressed_size);
        printf("  File name length: 0x%04x\n", local_file.local_file_header->file_name_length);
        printf("  Extra field length: 0x%04x\n", local_file.local_file_header->extra_field_length);
    } else {
        printf("Error parsing local file header\n");
    }

    result = H_PARSE(central_directory_parser, data, file_size, &central_directory);
    if (result == 0) {
        printf("Central directory header:\n");
        printf("  Signature: 0x%08x\n", central_directory.central_directory_header->signature);
        printf("  Version made by: 0x%04x\n", central_directory.central_directory_header->version_made_by);
        printf("  Version needed to extract: 0x%04x\n", central_directory.central_directory_header->version_needed_to_extract);
        printf("  General purpose bit flag: 0x%04x\n", central_directory.central_directory_header->general_purpose_bit_flag);
        printf("  Compression method: 0x%04x\n", central_directory.central_directory_header->compression_method);
        printf("  Last modified time: 0x%04x\n", central_directory.central_directory_header->last_modified_time);
        printf("  Last modified date: 0x%04x\n", central_directory.central_directory_header->last_modified_date);
        printf("  CRC-32: 0x%08x\n", central_directory.central_directory_header->crc_32);
        printf("  Compressed size: 0x%08x\n", central_directory.central_directory_header->compressed_size);
        printf("  Uncompressed size: 0x%08x\n", central_directory.central_directory_header->uncompressed_size);
        printf("  File name length: 0x%04x\n", central_directory.central_directory_header->file_name_length);
        printf("  Extra field length: 0x%04x\n", central_directory.central_directory_header->extra_field_length);
        printf("  File comment length: 0x%04x\n", central_directory.central_directory_header->file_comment_length);
        printf("  Disk number start: 0x%04x\n", central_directory.central_directory_header->disk_number_start);
        printf("  Internal attributes: 0x%04x\n", central_directory.central_directory_header->internal_attributes);
        printf("  External attributes: 0x%08x\n", central_directory.central_directory_header->external_attributes);
    } else {
        printf("Error parsing central directory header\n");
    }

    result = H_PARSE(end_of_central_directory_parser, data, file_size, &end_of_central_directory_record);
    if (result == 0) {
        printf("End of central directory record:\n");
        printf("  Signature: 0x%08x\n", end_of_central_directory_record.end_of_central_directory->signature);
        printf("  Number of this disk: 0x%04x\n", end_of_central_directory_record.end_of_central_directory->number_of_this_disk);
        printf("  Number of the disk where the central directory starts: 0x%04x\n", end_of_central_directory_record.end_of_central_directory->number_of_the_disk_where_the_central_directory_starts);
        printf("  Number of entries in the central directory on this disk: 0x%04x\n", end_of_central_directory_record.end_of_central_directory->number_of_entries_in_the_central_directory_on_this_disk);
        printf("  Total number of entries in the central directory: 0x%04x\n", end_of_central_directory_record.end_of_central_directory->total_number_of_entries_in_the_central_directory);
        printf("  Size of the central directory: 0x%08x\n", end_of_central_directory_record.end_of_central_directory->size_of_the_central_directory);
        printf("  Offset of the start of the central directory: 0x%08x\n", end_of_central_directory_record.end_of_central_directory->offset_of_the_start_of_the_central_directory);
        printf("  Comment length: 0x%04x\n", end_of_central_directory_record.end_of_central_directory->comment_length);
    } else {
        printf("Error parsing end of central directory record\n");
    }

    free(data);

    return 0;
}