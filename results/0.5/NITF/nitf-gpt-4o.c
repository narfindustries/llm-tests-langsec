#include <hammer/hammer.h>

HParser *create_nitf_parser() {
    // Define basic parsers
    HParser *uint8 = h_uint8();
    HParser *uint16 = h_uint16();
    HParser *uint32 = h_uint32();
    HParser *ascii_string = h_bind(h_repeat(uint8, 1, 80), h_ignore, h_return_str);
    
    // Define NITF header fields
    HParser *file_header = h_sequence(
        ascii_string, // File Type
        h_repeat(uint8, 9), // File Version
        h_repeat(uint8, 14), // File Title
        uint32, // File Length
        NULL
    );

    // Define Image Segment Subheader fields
    HParser *image_segment_subheader = h_sequence(
        h_repeat(uint8, 10), // Image Identifier
        uint16, // Image Date and Time
        uint16, // Image Security Classification
        uint32, // Image Source
        NULL
    );

    // Define Data Extension Segment Subheader fields
    HParser *data_extension_segment_subheader = h_sequence(
        h_repeat(uint8, 10), // Data Extension Identifier
        uint16, // Data Extension Date and Time
        uint16, // Data Extension Security Classification
        uint32, // Data Extension Source
        NULL
    );

    // Define the entire NITF file structure
    HParser *nitf_file = h_sequence(
        file_header,
        h_repeat(image_segment_subheader, 0, 10), // Up to 10 Image Segments
        h_repeat(data_extension_segment_subheader, 0, 5), // Up to 5 Data Extensions
        NULL
    );

    return nitf_file;
}

int main(int argc, char **argv) {
    // Initialize the parser
    HParser *parser = create_nitf_parser();

    // Load a NITF file for parsing
    FILE *file = fopen("example.nitf", "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    // Parse the file
    HParseResult *result = h_parse(parser, buffer, file_size);
    if (result) {
        printf("Parsing succeeded!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed.\n");
    }

    // Cleanup
    h_parser_free(parser);
    free(buffer);

    return 0;
}