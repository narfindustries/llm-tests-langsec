#include <hammer/hammer.h>

HParser *create_zip_parser() {
    // Local file header signature
    HParser *local_file_header_sig = h_sequence(
        h_uint8_val(0x50), // P
        h_uint8_val(0x4B), // K
        h_uint8_val(0x03), // 0x03
        h_uint8_val(0x04), // 0x04
        NULL
    );

    // Central directory file header signature
    HParser *central_dir_file_header_sig = h_sequence(
        h_uint8_val(0x50), // P
        h_uint8_val(0x4B), // K
        h_uint8_val(0x01), // 0x01
        h_uint8_val(0x02), // 0x02
        NULL
    );

    // End of central directory signature
    HParser *end_of_central_dir_sig = h_sequence(
        h_uint8_val(0x50), // P
        h_uint8_val(0x4B), // K
        h_uint8_val(0x05), // 0x05
        h_uint8_val(0x06), // 0x06
        NULL
    );

    // Local file header structure
    HParser *local_file_header = h_sequence(
        local_file_header_sig,
        h_uint16(), // Version needed to extract
        h_uint16(), // General purpose bit flag
        h_uint16(), // Compression method
        h_uint16(), // Last mod file time
        h_uint16(), // Last mod file date
        h_uint32(), // CRC-32
        h_uint32(), // Compressed size
        h_uint32(), // Uncompressed size
        h_uint16(), // File name length
        h_uint16(), // Extra field length
        h_data(h_uint16()), // File name
        h_data(h_uint16()), // Extra field
        NULL
    );

    // Central directory file header structure
    HParser *central_dir_file_header = h_sequence(
        central_dir_file_header_sig,
        h_uint16(), // Version made by
        h_uint16(), // Version needed to extract
        h_uint16(), // General purpose bit flag
        h_uint16(), // Compression method
        h_uint16(), // Last mod file time
        h_uint16(), // Last mod file date
        h_uint32(), // CRC-32
        h_uint32(), // Compressed size
        h_uint32(), // Uncompressed size
        h_uint16(), // File name length
        h_uint16(), // Extra field length
        h_uint16(), // File comment length
        h_uint16(), // Disk number start
        h_uint16(), // Internal file attributes
        h_uint32(), // External file attributes
        h_uint32(), // Relative offset of local header
        h_data(h_uint16()), // File name
        h_data(h_uint16()), // Extra field
        h_data(h_uint16()), // File comment
        NULL
    );

    // End of central directory record structure
    HParser *end_of_central_dir = h_sequence(
        end_of_central_dir_sig,
        h_uint16(), // Number of this disk
        h_uint16(), // Disk where central directory starts
        h_uint16(), // Number of central directory records on this disk
        h_uint16(), // Total number of central directory records
        h_uint32(), // Size of central directory (bytes)
        h_uint32(), // Offset of start of central directory, relative to start of archive
        h_uint16(), // Comment length
        h_data(h_uint16()), // Comment
        NULL
    );

    // ZIP file structure
    HParser *zip_file = h_many(
        h_choice(
            local_file_header,
            central_dir_file_header,
            end_of_central_dir,
            NULL
        )
    );

    return zip_file;
}

int main(int argc, char **argv) {
    HParser *zip_parser = create_zip_parser();
    // Use the parser with input data here
    h_parser_free(zip_parser);
    return 0;
}