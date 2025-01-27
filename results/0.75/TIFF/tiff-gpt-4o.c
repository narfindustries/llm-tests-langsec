#include <hammer/hammer.h>
#include <stdio.h>

HParser *create_tiff_parser() {
    // TIFF Header: 2 bytes for byte order indication, 2 bytes for 42 magic number, 4 bytes for IFD offset
    HParser *byte_order = h_choice(h_token("\x49\x49", 2), h_token("\x4D\x4D", 2), NULL);
    HParser *magic_number = h_token("\x2A\x00", 2); // assuming little-endian
    HParser *ifd_offset = h_uint32();

    HParser *tiff_header = h_sequence(byte_order, magic_number, ifd_offset, NULL);

    // TIFF IFD Entry: 2 bytes for tag, 2 bytes for type, 4 bytes for count, 4 bytes for value/offset
    HParser *tag = h_uint16();
    HParser *type = h_uint16();
    HParser *count = h_uint32();
    HParser *value_or_offset = h_uint32();
    
    HParser *ifd_entry = h_sequence(tag, type, count, value_or_offset, NULL);

    // TIFF IFD: 2 bytes for the number of entries, followed by the entries, and 4 bytes for next IFD offset
    HParser *num_entries = h_uint16();
    HParser *entries = h_repeat(ifd_entry, num_entries);
    HParser *next_ifd_offset = h_uint32();

    HParser *tiff_ifd = h_sequence(num_entries, entries, next_ifd_offset, NULL);

    // Complete TIFF parser: header followed by at least one IFD
    HParser *tiff_parser = h_sequence(tiff_header, h_many1(tiff_ifd), NULL);

    return tiff_parser;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <TIFF file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *data = (unsigned char *)malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *tiff_parser = create_tiff_parser();
    HParseResult *result = h_parse(tiff_parser, data, file_size);

    if (result->status == H_PARSE_OK) {
        printf("TIFF file parsed successfully.\n");
    } else {
        printf("Failed to parse TIFF file.\n");
    }

    h_parse_result_free(result);
    h_parser_free(tiff_parser);
    free(data);

    return 0;
}