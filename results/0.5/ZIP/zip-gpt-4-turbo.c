#include <hammer/hammer.h>
#include <hammer/glue.h>

// Helper function to create a fixed-length sequence parser
static inline HParser *h_sequence_n(const HParsedToken **tokens, size_t n, ...) {
    va_list ap;
    va_start(ap, n);
    HParser *p = h_sequence_v(n, ap);
    va_end(ap);
    return p;
}

// Define the parser for a ZIP file local file header
static HParser *zip_local_file_header() {
    return h_sequence_n((const HParsedToken *[]){
        h_bytes_value("PK\x03\x04", 4),  // Signature
        h_uint16_le(),                   // Version needed to extract
        h_uint16_le(),                   // General purpose bit flag
        h_uint16_le(),                   // Compression method
        h_uint16_le(),                   // Last mod file time
        h_uint16_le(),                   // Last mod file date
        h_uint32_le(),                   // CRC-32
        h_uint32_le(),                   // Compressed size
        h_uint32_le(),                   // Uncompressed size
        h_uint16_le(),                   // File name length
        h_uint16_le(),                   // Extra field length
        h_indirect(h_apply(h_uint16_le(), (HPayloadFn)h_bytes, NULL)),  // File name
        h_indirect(h_apply(h_uint16_le(), (HPayloadFn)h_bytes, NULL))   // Extra field
    }, 13);
}

// Define the parser for a ZIP file central directory file header
static HParser *zip_central_directory_file_header() {
    return h_sequence_n((const HParsedToken *[]){
        h_bytes_value("PK\x01\x02", 4),  // Signature
        h_uint16_le(),                   // Version made by
        h_uint16_le(),                   // Version needed to extract
        h_uint16_le(),                   // General purpose bit flag
        h_uint16_le(),                   // Compression method
        h_uint16_le(),                   // Last mod file time
        h_uint16_le(),                   // Last mod file date
        h_uint32_le(),                   // CRC-32
        h_uint32_le(),                   // Compressed size
        h_uint32_le(),                   // Uncompressed size
        h_uint16_le(),                   // File name length
        h_uint16_le(),                   // Extra field length
        h_uint16_le(),                   // File comment length
        h_uint16_le(),                   // Disk number start
        h_uint16_le(),                   // Internal file attributes
        h_uint32_le(),                   // External file attributes
        h_uint32_le(),                   // Relative offset of local header
        h_indirect(h_apply(h_uint16_le(), (HPayloadFn)h_bytes, NULL)),  // File name
        h_indirect(h_apply(h_uint16_le(), (HPayloadFn)h_bytes, NULL)),  // Extra field
        h_indirect(h_apply(h_uint16_le(), (HPayloadFn)h_bytes, NULL))   // File comment
    }, 19);
}

// Define the parser for a ZIP file end of central directory record
static HParser *zip_end_of_central_dir_record() {
    return h_sequence_n((const HParsedToken *[]){
        h_bytes_value("PK\x05\x06", 4),  // Signature
        h_uint16_le(),                   // Number of this disk
        h_uint16_le(),                   // Disk where central directory starts
        h_uint16_le(),                   // Number of central directory records on this disk
        h_uint16_le(),                   // Total number of central directory records
        h_uint32_le(),                   // Size of central directory (bytes)
        h_uint32_le(),                   // Offset of start of central directory, relative to start of archive
        h_uint16_le(),                   // ZIP file comment length
        h_indirect(h_apply(h_uint16_le(), (HPayloadFn)h_bytes, NULL))   // ZIP file comment
    }, 9);
}

// Define the parser for the entire ZIP file
static HParser *zip_file() {
    return h_sequence_n((const HParsedToken *[]){
        h_many(zip_local_file_header()),             // Parse multiple local file headers
        h_many(zip_central_directory_file_header()), // Parse multiple central directory file headers
        zip_end_of_central_dir_record()              // Parse the end of central directory record
    }, 3);
}

int main(int argc, char **argv) {
    HParser *parser = zip_file();
    HParseResult *result = h_parse(parser, (const uint8_t *)input_data, input_length);
    if (result) {
        printf("ZIP file parsed successfully.\n");
    } else {
        printf("Failed to parse ZIP file.\n");
    }
    h_parse_result_free(result);
    h_parser_unref(parser);
    return 0;
}