#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// ZIP file format parser using Hammer

// Signature constants
#define LOCAL_FILE_HEADER_SIG 0x04034B50
#define CENTRAL_DIR_HEADER_SIG 0x02014B50
#define END_CENTRAL_DIR_SIG 0x06054B50

// Compression methods
typedef enum {
    NO_COMPRESSION = 0,
    DEFLATE = 8,
    BZIP2 = 12,
    LZMA = 14
} CompressionMethod;

// Bit flag structure
typedef struct {
    uint8_t encryption : 1;
    uint8_t compression_option1 : 1;
    uint8_t compression_option2 : 1;
    uint8_t data_descriptor : 1;
    uint16_t reserved : 12;
} GeneralBitFlag;

// Local File Header structure
typedef struct {
    uint32_t signature;
    uint16_t version_needed;
    GeneralBitFlag bit_flag;
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

// Central Directory File Header
typedef struct {
    uint32_t signature;
    uint16_t version_made_by;
    uint16_t version_needed;
    GeneralBitFlag bit_flag;
    CompressionMethod compression_method;
    uint16_t last_mod_time;
    uint16_t last_mod_date;
    uint32_t crc32;
    uint32_t compressed_size;
    uint32_t uncompressed_size;
    uint16_t filename_length;
    uint16_t extra_field_length;
    uint16_t comment_length;
    uint16_t disk_number_start;
    uint16_t internal_file_attrs;
    uint32_t external_file_attrs;
    uint32_t local_header_offset;
    char* filename;
    uint8_t* extra_field;
    char* file_comment;
} CentralDirectoryFileHeader;

// End of Central Directory Record
typedef struct {
    uint32_t signature;
    uint16_t disk_number;
    uint16_t central_dir_disk;
    uint16_t central_dir_entries_disk;
    uint16_t total_central_dir_entries;
    uint32_t central_dir_size;
    uint32_t central_dir_offset;
    uint16_t comment_length;
    char* comment;
} EndCentralDirectoryRecord;

// Hammer parser definitions
static HParser* zip_signature_parser;
static HParser* local_file_header_parser;
static HParser* central_dir_header_parser;
static HParser* end_central_dir_parser;

// Parser initialization function
void init_zip_parsers() {
    // Signature parser
    zip_signature_parser = h_uint32();

    // Local File Header parser
    local_file_header_parser = h_sequence(
        h_uint32(),   // signature
        h_uint16(),   // version needed
        h_bits(16, false),  // bit flags
        h_uint16(),   // compression method
        h_uint16(),   // last mod time
        h_uint16(),   // last mod date
        h_uint32(),   // crc32
        h_uint32(),   // compressed size
        h_uint32(),   // uncompressed size
        h_uint16(),   // filename length
        h_uint16(),   // extra field length
        h_many(h_ch_range(0x20, 0x7E)),  // filename
        h_many(h_uint8())  // extra field
    );

    // Central Directory Header parser
    central_dir_header_parser = h_sequence(
        h_uint32(),   // signature
        h_uint16(),   // version made by
        h_uint16(),   // version needed
        h_bits(16, false),  // bit flags
        h_uint16(),   // compression method
        h_uint16(),   // last mod time
        h_uint16(),   // last mod date
        h_uint32(),   // crc32
        h_uint32(),   // compressed size
        h_uint32(),   // uncompressed size
        h_uint16(),   // filename length
        h_uint16(),   // extra field length
        h_uint16(),   // comment length
        h_uint16(),   // disk number start
        h_uint16(),   // internal file attributes
        h_uint32(),   // external file attributes
        h_uint32(),   // local header offset
        h_many(h_ch_range(0x20, 0x7E)),  // filename
        h_many(h_uint8()),  // extra field
        h_many(h_ch_range(0x20, 0x7E))  // file comment
    );

    // End of Central Directory parser
    end_central_dir_parser = h_sequence(
        h_uint32(),   // signature
        h_uint16(),   // disk number
        h_uint16(),   // central dir disk
        h_uint16(),   // central dir entries on disk
        h_uint16(),   // total central dir entries
        h_uint32(),   // central dir size
        h_uint32(),   // central dir offset
        h_uint16(),   // comment length
        h_many(h_ch_range(0x20, 0x7E))  // comment
    );
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <zip_file>\n", argv[0]);
        return 1;
    }

    // Initialize Hammer parsers
    init_zip_parsers();

    // Open file
    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    // Get file size
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    // Read file contents
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

    // Parse ZIP file
    HParseResult* result = h_parse(local_file_header_parser, buffer, file_size);
    if (result) {
        printf("ZIP file parsed successfully\n");
    } else {
        printf("ZIP file parsing failed\n");
    }

    // Cleanup
    free(buffer);
    fclose(file);
    return 0;
}