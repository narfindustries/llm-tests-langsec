#include <hammer/hammer.h>

HParser *create_nitf_parser() {
    // Define basic parsers
    HParser *uint8 = h_uint8();
    HParser *uint16 = h_uint16();
    HParser *uint32 = h_uint32();
    HParser *ascii = h_ch_range(0x20, 0x7E); // Printable ASCII

    // Define NITF header fields
    HParser *file_header = h_sequence(
        h_repeat(ascii, 9),  // File Type
        h_repeat(ascii, 5),  // Version
        uint16,              // Complexity Level
        uint32,              // File Size
        h_repeat(ascii, 10), // File Designator
        h_repeat(ascii, 14), // File Classification
        h_repeat(ascii, 20), // File Security
        NULL
    );

    // Define NITF image segment fields
    HParser *image_segment = h_sequence(
        h_repeat(ascii, 6),  // Image Identifier
        uint32,              // Image Data Size
        h_repeat(ascii, 10), // Image Security
        NULL
    );

    // Define NITF data structure
    HParser *nitf_parser = h_sequence(
        file_header,
        h_many(image_segment),
        NULL
    );

    return nitf_parser;
}

int main(int argc, char **argv) {
    HParser *nitf_parser = create_nitf_parser();
    HParseResult *result;
    const uint8_t *data;
    size_t data_len;

    // Load your NITF data into `data` and set `data_len` accordingly

    result = h_parse(nitf_parser, data, data_len);

    if (result) {
        // Successfully parsed
        h_parse_result_free(result);
    } else {
        // Handle parse error
    }

    h_parser_free(nitf_parser);
    return 0;
}