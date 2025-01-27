#include <hammer/hammer.h>
#include <stdio.h>

const HParser* zip_local_file_header(void) {
    return h_sequence(
        h_token((const uint8_t*)"\x50\x4b\x03\x04", 4),  // Local file header signature
        h_uint16(),                                       // Version needed to extract
        h_uint16(),                                       // General purpose bit flag
        h_uint16(),                                       // Compression method
        h_uint16(),                                       // Last mod file time
        h_uint16(),                                       // Last mod file date
        h_uint32(),                                       // CRC-32
        h_uint32(),                                       // Compressed size
        h_uint32(),                                       // Uncompressed size
        h_length_value(h_uint16(), h_uint8()),           // File name
        h_length_value(h_uint16(), h_uint8()),           // Extra field
        NULL);
}

const HParser* zip_central_directory(void) {
    return h_sequence(
        h_token((const uint8_t*)"\x50\x4b\x01\x02", 4),  // Central directory signature
        h_uint16(),                                       // Version made by
        h_uint16(),                                       // Version needed to extract
        h_uint16(),                                       // General purpose bit flag
        h_uint16(),                                       // Compression method
        h_uint16(),                                       // Last mod file time
        h_uint16(),                                       // Last mod file date
        h_uint32(),                                       // CRC-32
        h_uint32(),                                       // Compressed size
        h_uint32(),                                       // Uncompressed size
        h_length_value(h_uint16(), h_uint8()),           // File name length
        h_length_value(h_uint16(), h_uint8()),           // Extra field length
        h_length_value(h_uint16(), h_uint8()),           // File comment length
        h_uint16(),                                       // Disk number start
        h_uint16(),                                       // Internal file attributes
        h_uint32(),                                       // External file attributes
        h_uint32(),                                       // Relative offset of local header
        NULL);
}

const HParser* zip_end_of_central_directory(void) {
    return h_sequence(
        h_token((const uint8_t*)"\x50\x4b\x05\x06", 4),  // End of central directory signature
        h_uint16(),                                       // Number of this disk
        h_uint16(),                                       // Disk where central directory starts
        h_uint16(),                                       // Number of central directory records on this disk
        h_uint16(),                                       // Total number of central directory records
        h_uint32(),                                       // Size of central directory
        h_uint32(),                                       // Offset of central directory
        h_length_value(h_uint16(), h_uint8()),           // Comment
        NULL);
}

const HParser* zip_parser(void) {
    return h_sequence(
        h_many(zip_local_file_header()),
        h_many(zip_central_directory()),
        zip_end_of_central_directory(),
        NULL);
}

HParser* init_parser(void) {
    return h_indirect(zip_parser);
}