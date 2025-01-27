#include <hammer/hammer.h>
#include <hammer/glue.h>

// Define the structures for a ZIP file
static HParser *zip_end_of_central_dir;
static HParser *zip_central_dir_file_header;
static HParser *zip_local_file_header;

// ZIP end of central directory record
static void init_zip_end_of_central_dir() {
    zip_end_of_central_dir = h_sequence(
        h_string("PK\x05\x06", 4),                 // End of central dir signature
        h_uint16(),                                // Number of this disk
        h_uint16(),                                // Disk where central directory starts
        h_uint16(),                                // Number of central directory records on this disk
        h_uint16(),                                // Total number of central directory records
        h_uint32(),                                // Size of central directory (bytes)
        h_uint32(),                                // Offset of start of central directory, relative to start of archive
        h_uint16(),                                // ZIP file comment length
        h_bytes(h_uint16()),                       // ZIP file comment
        NULL
    );
}

// ZIP central directory file header
static void init_zip_central_dir_file_header() {
    zip_central_dir_file_header = h_sequence(
        h_string("PK\x01\x02", 4),                 // Central file header signature
        h_uint16(),                                // Version made by
        h_uint16(),                                // Version needed to extract
        h_uint16(),                                // General purpose bit flag
        h_uint16(),                                // Compression method
        h_uint16(),                                // Last mod file time
        h_uint16(),                                // Last mod file date
        h_uint32(),                                // CRC-32
        h_uint32(),                                // Compressed size
        h_uint32(),                                // Uncompressed size
        h_uint16(),                                // File name length
        h_uint16(),                                // Extra field length
        h_uint16(),                                // File comment length
        h_uint16(),                                // Disk number start
        h_uint16(),                                // Internal file attributes
        h_uint32(),                                // External file attributes
        h_uint32(),                                // Relative offset of local file header
        h_bytes(h_uint16()),                       // File name
        h_bytes(h_uint16()),                       // Extra field
        h_bytes(h_uint16()),                       // File comment
        NULL
    );
}

// ZIP local file header
static void init_zip_local_file_header() {
    zip_local_file_header = h_sequence(
        h_string("PK\x03\x04", 4),                 // Local file header signature
        h_uint16(),                                // Version needed to extract
        h_uint16(),                                // General purpose bit flag
        h_uint16(),                                // Compression method
        h_uint16(),                                // Last mod file time
        h_uint16(),                                // Last mod file date
        h_uint32(),                                // CRC-32
        h_uint32(),                                // Compressed size
        h_uint32(),                                // Uncompressed size
        h_uint16(),                                // File name length
        h_uint16(),                                // Extra field length
        h_bytes(h_uint16()),                       // File name
        h_bytes(h_uint16()),                       // Extra field
        NULL
    );
}

// Complete ZIP parser
static void init_zip_parser() {
    init_zip_end_of_central_dir();
    init_zip_central_dir_file_header();
    init_zip_local_file_header();

    HParser *zip_file = h_sequence(
        h_many(zip_local_file_header),             // Parse multiple local file headers
        h_many(zip_central_dir_file_header),       // Parse multiple central directory file headers
        zip_end_of_central_dir,                    // End of central directory record
        NULL
    );

    // Compile the parser
    HParseResult *result = h_parse(zip_file, input_buffer, input_length);
    if (result) {
        // Successfully parsed
        printf("ZIP file parsed successfully.\n");
    } else {
        // Failed to parse
        printf("Failed to parse ZIP file.\n");
    }
}

int main(int argc, char **argv) {
    // Initialize the ZIP parser
    init_zip_parser();

    return 0;
}