#include <hammer/hammer.h>

// Define ZIP file constants
#define ZIP_LOCAL_FILE_HEADER_SIGNATURE 0x04034b50
#define ZIP_CENTRAL_DIRECTORY_SIGNATURE 0x02014b50
#define ZIP_END_OF_CENTRAL_DIRECTORY_SIGNATURE 0x06054b50

// Define the ZIP file structures
typedef struct {
    uint32_t signature;
    uint16_t version;
    uint16_t flags;
    uint16_t compression;
    uint16_t mod_time;
    uint16_t mod_date;
    uint32_t crc32;
    uint32_t compressed_size;
    uint32_t uncompressed_size;
    uint16_t filename_length;
    uint16_t extra_field_length;
    // Followed by filename and extra field
} zip_local_file_header_t;

typedef struct {
    uint32_t signature;
    uint16_t version_made_by;
    uint16_t version_needed;
    uint16_t flags;
    uint16_t compression;
    uint16_t mod_time;
    uint16_t mod_date;
    uint32_t crc32;
    uint32_t compressed_size;
    uint32_t uncompressed_size;
    uint16_t filename_length;
    uint16_t extra_field_length;
    uint16_t file_comment_length;
    uint16_t disk_number_start;
    uint16_t internal_file_attributes;
    uint32_t external_file_attributes;
    uint32_t local_header_offset;
    // Followed by filename, extra field, and file comment
} zip_central_directory_file_header_t;

typedef struct {
    uint32_t signature;
    uint16_t disk_number;
    uint16_t central_directory_disk;
    uint16_t num_entries_disk;
    uint16_t total_entries;
    uint32_t central_directory_size;
    uint32_t central_directory_offset;
    uint16_t comment_length;
    // Followed by comment
} zip_end_of_central_directory_record_t;

// Define parsers for each structure
HParser *zip_local_file_header_parser = h_sequence(
    h_uint32(ZIP_LOCAL_FILE_HEADER_SIGNATURE),
    h_uint16(), // version
    h_uint16(), // flags
    h_uint16(), // compression
    h_uint16(), // mod_time
    h_uint16(), // mod_date
    h_uint32(), // crc32
    h_uint32(), // compressed_size
    h_uint32(), // uncompressed_size
    h_uint16(), // filename_length
    h_uint16(), // extra_field_length
    NULL
);

HParser *zip_central_directory_file_header_parser = h_sequence(
    h_uint32(ZIP_CENTRAL_DIRECTORY_SIGNATURE),
    h_uint16(), // version_made_by
    h_uint16(), // version_needed
    h_uint16(), // flags
    h_uint16(), // compression
    h_uint16(), // mod_time
    h_uint16(), // mod_date
    h_uint32(), // crc32
    h_uint32(), // compressed_size
    h_uint32(), // uncompressed_size
    h_uint16(), // filename_length
    h_uint16(), // extra_field_length
    h_uint16(), // file_comment_length
    h_uint16(), // disk_number_start
    h_uint16(), // internal_file_attributes
    h_uint32(), // external_file_attributes
    h_uint32(), // local_header_offset
    NULL
);

HParser *zip_end_of_central_directory_record_parser = h_sequence(
    h_uint32(ZIP_END_OF_CENTRAL_DIRECTORY_SIGNATURE),
    h_uint16(), // disk_number
    h_uint16(), // central_directory_disk
    h_uint16(), // num_entries_disk
    h_uint16(), // total_entries
    h_uint32(), // central_directory_size
    h_uint32(), // central_directory_offset
    h_uint16(), // comment_length
    NULL
);

// Define the ZIP file parser
HParser *zip_parser = h_choice(
    zip_local_file_header_parser,
    zip_central_directory_file_header_parser,
    zip_end_of_central_directory_record_parser,
    NULL
);