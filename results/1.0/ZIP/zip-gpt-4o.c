#include <hammer/hammer.h>

HParser *create_zip_parser() {
    // A simple ZIP file parser specification using Hammer

    // Local parsers
    HParser *local_file_header_signature = h_sequence(
        h_uint32(0x04034b50), // PK\003\004
        NULL
    );

    HParser *version_needed = h_uint16();
    HParser *general_purpose_bit_flag = h_uint16();
    HParser *compression_method = h_uint16();
    HParser *file_mod_time = h_uint16();
    HParser *file_mod_date = h_uint16();
    HParser *crc32 = h_uint32();
    HParser *compressed_size = h_uint32();
    HParser *uncompressed_size = h_uint32();
    HParser *file_name_length = h_uint16();
    HParser *extra_field_length = h_uint16();

    HParser *local_file_header = h_sequence(
        local_file_header_signature,
        version_needed,
        general_purpose_bit_flag,
        compression_method,
        file_mod_time,
        file_mod_date,
        crc32,
        compressed_size,
        uncompressed_size,
        file_name_length,
        extra_field_length,
        h_data(UINT16(file_name_length)), // file name
        h_data(UINT16(extra_field_length)), // extra field
        NULL
    );

    // Central directory structure similar approach can be defined here
    // For simplicity, let's just parse local file headers in the ZIP

    // Zip parser that accepts one or more local file headers
    HParser *zip_parser = h_many1(local_file_header);

    return zip_parser;
}

int main() {
    HParser *zip_parser = create_zip_parser();

    // Use the parser
    // (Without a specific input and output part here as the example focuses on the specification)

    // Free resources
    h_parser_free(zip_parser);

    return 0;
}