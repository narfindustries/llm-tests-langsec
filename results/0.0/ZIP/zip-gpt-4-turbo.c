#include <hammer/hammer.h>
#include <hammer/glue.h>

// Define the basic building blocks of a ZIP file
static HParser *uint16 = h_uint16_le();
static HParser *uint32 = h_uint32_le();

// Local file header
static HParser *local_file_header = h_sequence(
    h_bytes_lit("PK\x03\x04", 4),  // Local file header signature
    uint16,                        // Version needed to extract
    uint16,                        // General purpose bit flag
    uint16,                        // Compression method
    uint16,                        // Last mod file time
    uint16,                        // Last mod file date
    uint32,                        // CRC-32
    uint32,                        // Compressed size
    uint32,                        // Uncompressed size
    uint16,                        // File name length
    uint16,                        // Extra field length
    h_indirect(h_apply(h_length_value(h_uint16_le()), h_bytes)), // File name
    h_indirect(h_apply(h_length_value(h_uint16_le()), h_bytes)), // Extra field
    NULL
);

// Central directory file header
static HParser *central_directory_file_header = h_sequence(
    h_bytes_lit("PK\x01\x02", 4),  // Central file header signature
    uint16,                        // Version made by
    uint16,                        // Version needed to extract
    uint16,                        // General purpose bit flag
    uint16,                        // Compression method
    uint16,                        // Last mod file time
    uint16,                        // Last mod file date
    uint32,                        // CRC-32
    uint32,                        // Compressed size
    uint32,                        // Uncompressed size
    uint16,                        // File name length
    uint16,                        // Extra field length
    uint16,                        // File comment length
    uint16,                        // Disk number start
    uint16,                        // Internal file attributes
    uint32,                        // External file attributes
    uint32,                        // Relative offset of local header
    h_indirect(h_apply(h_length_value(h_uint16_le()), h_bytes)), // File name
    h_indirect(h_apply(h_length_value(h_uint16_le()), h_bytes)), // Extra field
    h_indirect(h_apply(h_length_value(h_uint16_le()), h_bytes)), // File comment
    NULL
);

// End of central directory record
static HParser *end_of_central_dir_record = h_sequence(
    h_bytes_lit("PK\x05\x06", 4),  // End of central dir signature
    uint16,                        // Number of this disk
    uint16,                        // Disk where central directory starts
    uint16,                        // Number of central directory records on this disk
    uint16,                        // Total number of central directory records
    uint32,                        // Size of central directory (bytes)
    uint32,                        // Offset of start of central directory, relative to start of archive
    uint16,                        // ZIP file comment length
    h_indirect(h_apply(h_length_value(h_uint16_le()), h_bytes)), // ZIP file comment
    NULL
);

// Complete ZIP file parser
static HParser *zip_file = h_sequence(
    h_many(local_file_header),     // Parse multiple local file headers
    h_many(central_directory_file_header), // Parse multiple central directory file headers
    end_of_central_dir_record,     // End of central directory record
    NULL
);

int main(int argc, char **argv) {
    HParser *parser = zip_file;
    HParseResult *result = h_parse(parser, (const uint8_t *)argv[1], strlen(argv[1]));
    if (result) {
        printf("Parse successful!\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        printf("Parse failed!\n");
    }
    h_parse_result_free(result);
    return 0;
}