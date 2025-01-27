#include <hammer/hammer.h>

HParser *tiff_header_parser() {
    HParser *byte_order = h_choice(
        h_token("II", 2), // Little-endian
        h_token("MM", 2), // Big-endian
        NULL
    );

    HParser *version = h_uint16();

    HParser *ifd_offset = h_uint32();

    return h_sequence(byte_order, version, ifd_offset, NULL);
}

HParser *tiff_tag_parser() {
    HParser *tag_id = h_uint16();
    HParser *data_type = h_uint16();
    HParser *num_values = h_uint32();
    HParser *value_offset = h_uint32();

    return h_sequence(tag_id, data_type, num_values, value_offset, NULL);
}

HParser *tiff_ifd_parser() {
    HParser *num_entries = h_uint16();
    HParser *tags = h_repeat(tiff_tag_parser(), num_entries);
    HParser *next_ifd_offset = h_uint32();

    return h_sequence(num_entries, tags, next_ifd_offset, NULL);
}

HParser *tiff_parser() {
    HParser *header = tiff_header_parser();
    HParser *ifd = tiff_ifd_parser();

    return h_sequence(header, ifd, NULL);
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <tiff-file>\n", argv[0]);
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

    uint8_t *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *parser = tiff_parser();
    HParseResult *result = h_parse(parser, data, file_size);

    if (result) {
        printf("TIFF file parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse TIFF file.\n");
    }

    h_parser_free(parser);
    free(data);

    return 0;
}