#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef struct {
    uint32_t signature;
    uint16_t version;
    uint16_t flags;
    uint16_t compression_method;
    uint16_t last_mod_file_time;
    uint16_t last_mod_file_date;
    uint32_t crc_32;
    uint32_t compressed_size;
    uint32_t uncompressed_size;
    uint16_t filename_length;
    uint16_t extra_field_length;
    uint8_t* filename;
    uint8_t* extra_field;
} LocalFileHeader;

typedef struct {
    uint32_t signature;
    uint32_t crc_32;
    uint32_t compressed_size;
    uint32_t uncompressed_size;
} DataDescriptor;

typedef struct {
    uint32_t signature;
    uint16_t version_made_by;
    uint16_t version_needed_to_extract;
    uint16_t flags;
    uint16_t compression_method;
    uint16_t last_mod_file_time;
    uint16_t last_mod_file_date;
    uint32_t crc_32;
    uint32_t compressed_size;
    uint32_t uncompressed_size;
    uint16_t filename_length;
    uint16_t extra_field_length;
    uint16_t comment_length;
    uint16_t disk_number_start;
    uint16_t internal_file_attributes;
    uint32_t external_file_attributes;
    uint32_t local_header_offset;
    uint8_t* filename;
    uint8_t* extra_field;
    uint8_t* comment;
} CentralDirectoryEntry;

typedef struct {
    uint32_t signature;
    uint16_t number_of_this_disk;
    uint16_t disk_where_central_directory_starts;
    uint16_t number_of_central_directory_entries_on_this_disk;
    uint16_t total_number_of_central_directory_entries;
    uint32_t size_of_central_directory;
    uint32_t offset_of_start_of_central_directory;
    uint16_t zipfile_comment_length;
    uint8_t* zipfile_comment;
} EndOfCentralDirectoryRecord;

typedef struct {
    LocalFileHeader* local_file_header;
    DataDescriptor* data_descriptor;
    uint8_t* file_data;
} File;

typedef struct {
    CentralDirectoryEntry** central_directory_entries;
    uint16_t number_of_central_directory_entries;
    EndOfCentralDirectoryRecord* end_of_central_directory_record;
} ZipFile;

#define HAMMER_PARSER_T struct hammer_parser_t
#define HAMMER_PARSER_TNX struct hammer_parser_tnx

HAMMER_PARSER_T* local_file_header_parser() {
    HAMMER_PARSER_T* signature = hammer_uint32_p(0x04034b50);
    HAMMER_PARSER_T* version = hammer_uint16_p();
    HAMMER_PARSER_T* flags = hammer_uint16_p();
    HAMMER_PARSER_T* compression_method = hammer_uint16_p();
    HAMMER_PARSER_T* last_mod_file_time = hammer_uint16_p();
    HAMMER_PARSER_T* last_mod_file_date = hammer_uint16_p();
    HAMMER_PARSER_T* crc_32 = hammer_uint32_p();
    HAMMER_PARSER_T* compressed_size = hammer_uint32_p();
    HAMMER_PARSER_T* uncompressed_size = hammer_uint32_p();
    HAMMER_PARSER_T* filename_length = hammer_uint16_p();
    HAMMER_PARSER_T* extra_field_length = hammer_uint16_p();

    HAMMER_PARSER_T* filename = hammer_string_p(filename_length);
    HAMMER_PARSER_T* extra_field = hammer_string_p(extra_field_length);

    LocalFileHeader* local_file_header = malloc(sizeof(LocalFileHeader));

    HAMMER_PARSER_T* parser = hammer_struct_p(&local_file_header->signature,
        &local_file_header->version,
        &local_file_header->flags,
        &local_file_header->compression_method,
        &local_file_header->last_mod_file_time,
        &local_file_header->last_mod_file_date,
        &local_file_header->crc_32,
        &local_file_header->compressed_size,
        &local_file_header->uncompressed_size,
        &local_file_header->filename_length,
        &local_file_header->extra_field_length,
        &local_file_header->filename,
        &local_file_header->extra_field);

    return parser;
}

HAMMER_PARSER_T* data_descriptor_parser() {
    HAMMER_PARSER_T* signature = hammer_uint32_p(0x08074b50);
    HAMMER_PARSER_T* crc_32 = hammer_uint32_p();
    HAMMER_PARSER_T* compressed_size = hammer_uint32_p();
    HAMMER_PARSER_T* uncompressed_size = hammer_uint32_p();

    DataDescriptor* data_descriptor = malloc(sizeof(DataDescriptor));

    HAMMER_PARSER_T* parser = hammer_struct_p(&data_descriptor->signature,
        &data_descriptor->crc_32,
        &data_descriptor->compressed_size,
        &data_descriptor->uncompressed_size);

    return parser;
}

HAMMER_PARSER_T* file_parser() {
    HAMMER_PARSER_T* local_file_header = local_file_header_parser();
    HAMMER_PARSER_T* data_descriptor = data_descriptor_parser();
    HAMMER_PARSER_T* file_data = hammer_string_p(100);

    File* file = malloc(sizeof(File));
    file->local_file_header = malloc(sizeof(LocalFileHeader));
    file->data_descriptor = malloc(sizeof(DataDescriptor));
    file->file_data = file_data;

    HAMMER_PARSER_T* parser = hammer_struct_p(&file->local_file_header->signature,
        &file->local_file_header->version,
        &file->local_file_header->flags,
        &file->local_file_header->compression_method,
        &file->local_file_header->last_mod_file_time,
        &file->local_file_header->last_mod_file_date,
        &file->local_file_header->crc_32,
        &file->local_file_header->compressed_size,
        &file->local_file_header->uncompressed_size,
        &file->local_file_header->filename_length,
        &file->local_file_header->extra_field_length,
        &file->local_file_header->filename,
        &file->local_file_header->extra_field,
        &file->data_descriptor->signature,
        &file->data_descriptor->crc_32,
        &file->data_descriptor->compressed_size,
        &file->data_descriptor->uncompressed_size,
        &file->file_data);

    return parser;
}

HAMMER_PARSER_T* central_directory_entry_parser() {
    HAMMER_PARSER_T* signature = hammer_uint32_p(0x02014b50);
    HAMMER_PARSER_T* version_made_by = hammer_uint16_p();
    HAMMER_PARSER_T* version_needed_to_extract = hammer_uint16_p();
    HAMMER_PARSER_T* flags = hammer_uint16_p();
    HAMMER_PARSER_T* compression_method = hammer_uint16_p();
    HAMMER_PARSER_T* last_mod_file_time = hammer_uint16_p();
    HAMMER_PARSER_T* last_mod_file_date = hammer_uint16_p();
    HAMMER_PARSER_T* crc_32 = hammer_uint32_p();
    HAMMER_PARSER_T* compressed_size = hammer_uint32_p();
    HAMMER_PARSER_T* uncompressed_size = hammer_uint32_p();
    HAMMER_PARSER_T* filename_length = hammer_uint16_p();
    HAMMER_PARSER_T* extra_field_length = hammer_uint16_p();
    HAMMER_PARSER_T* comment_length = hammer_uint16_p();
    HAMMER_PARSER_T* disk_number_start = hammer_uint16_p();
    HAMMER_PARSER_T* internal_file_attributes = hammer_uint16_p();
    HAMMER_PARSER_T* external_file_attributes = hammer_uint32_p();
    HAMMER_PARSER_T* local_header_offset = hammer_uint32_p();

    HAMMER_PARSER_T* filename = hammer_string_p(filename_length);
    HAMMER_PARSER_T* extra_field = hammer_string_p(extra_field_length);
    HAMMER_PARSER_T* comment = hammer_string_p(comment_length);

    CentralDirectoryEntry* central_directory_entry = malloc(sizeof(CentralDirectoryEntry));

    HAMMER_PARSER_T* parser = hammer_struct_p(&central_directory_entry->signature,
        &central_directory_entry->version_made_by,
        &central_directory_entry->version_needed_to_extract,
        &central_directory_entry->flags,
        &central_directory_entry->compression_method,
        &central_directory_entry->last_mod_file_time,
        &central_directory_entry->last_mod_file_date,
        &central_directory_entry->crc_32,
        &central_directory_entry->compressed_size,
        &central_directory_entry->uncompressed_size,
        &central_directory_entry->filename_length,
        &central_directory_entry->extra_field_length,
        &central_directory_entry->comment_length,
        &central_directory_entry->disk_number_start,
        &central_directory_entry->internal_file_attributes,
        &central_directory_entry->external_file_attributes,
        &central_directory_entry->local_header_offset,
        &central_directory_entry->filename,
        &central_directory_entry->extra_field,
        &central_directory_entry->comment);

    return parser;
}

HAMMER_PARSER_T* end_of_central_directory_record_parser() {
    HAMMER_PARSER_T* signature = hammer_uint32_p(0x06054b50);
    HAMMER_PARSER_T* number_of_this_disk = hammer_uint16_p();
    HAMMER_PARSER_T* disk_where_central_directory_starts = hammer_uint16_p();
    HAMMER_PARSER_T* number_of_central_directory_entries_on_this_disk = hammer_uint16_p();
    HAMMER_PARSER_T* total_number_of_central_directory_entries = hammer_uint16_p();
    HAMMER_PARSER_T* size_of_central_directory = hammer_uint32_p();
    HAMMER_PARSER_T* offset_of_start_of_central_directory = hammer_uint32_p();
    HAMMER_PARSER_T* zipfile_comment_length = hammer_uint16_p();

    HAMMER_PARSER_T* zipfile_comment = hammer_string_p(zipfile_comment_length);

    EndOfCentralDirectoryRecord* end_of_central_directory_record = malloc(sizeof(EndOfCentralDirectoryRecord));

    HAMMER_PARSER_T* parser = hammer_struct_p(&end_of_central_directory_record->signature,
        &end_of_central_directory_record->number_of_this_disk,
        &end_of_central_directory_record->disk_where_central_directory_starts,
        &end_of_central_directory_record->number_of_central_directory_entries_on_this_disk,
        &end_of_central_directory_record->total_number_of_central_directory_entries,
        &end_of_central_directory_record->size_of_central_directory,
        &end_of_central_directory_record->offset_of_start_of_central_directory,
        &end_of_central_directory_record->zipfile_comment_length,
        &end_of_central_directory_record->zipfile_comment);

    return parser;
}

int main(int argc, char** argv) {
    if (argc < 2) {
        printf("Usage: %s <zip_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (file == NULL) {
        printf("Error opening file: %s\n", argv[1]);
        return 1;
    }

    HAMMER_PARSER_T* parser = hammer_parser_init();

    ZipFile* zip_file = malloc(sizeof(ZipFile));

    HAMMER_PARSER_T* end_of_central_directory_record_parser_t = end_of_central_directory_record_parser();
    hammer_parse(parser, end_of_central_directory_record_parser_t, &zip_file->end_of_central_directory_record);

    CentralDirectoryEntry** central_directory_entries = malloc(sizeof(CentralDirectoryEntry*) * zip_file->end_of_central_directory_record->total_number_of_central_directory_entries);
    for (int i = 0; i < zip_file->end_of_central_directory_record->total_number_of_central_directory_entries; i++) {
        HAMMER_PARSER_T* central_directory_entry_parser_t = central_directory_entry_parser();
        hammer_parse(parser, central_directory_entry_parser_t, &central_directory_entries[i]);
    }
    zip_file->central_directory_entries = central_directory_entries;
    zip_file->number_of_central_directory_entries = zip_file->end_of_central_directory_record->total_number_of_central_directory_entries;

    fclose(file);

    printf("Number of central directory entries: %u\n", zip_file->number_of_central_directory_entries);

    for (int i = 0; i < zip_file->number_of_central_directory_entries; i++) {
        printf("Filename: %s\n", (char*)zip_file->central_directory_entries[i]->filename);
        printf("Compression method: %u\n", zip_file->central_directory_entries[i]->compression_method);
        printf("Compressed size: %u\n", zip_file->central_directory_entries[i]->compressed_size);
        printf("Uncompressed size: %u\n", zip_file->central_directory_entries[i]->uncompressed_size);
        printf("\n");
    }

    return 0;
}