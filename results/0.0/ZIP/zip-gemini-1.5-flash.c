#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

// Helper function to read a specific number of bytes
static int read_bytes(FILE *fp, void *buffer, size_t count) {
    return fread(buffer, 1, count, fp) == count;
}

// Define parsers for individual fields
static HParser uint16_parser = h_map(h_uint8_be(2), h_from_big_endian16);
static HParser uint32_parser = h_map(h_uint8_be(4), h_from_big_endian32);
static HParser uint32_crc_parser = uint32_parser;
static HParser dos_time_parser = h_uint8(4);
static HParser filename_len_parser = uint16_parser;
static HParser extra_field_len_parser = uint16_parser;
static HParser file_comment_len_parser = uint16_parser;
static HParser signature_parser = h_map(h_uint8_be(4), h_from_big_endian32);

// Parser for the local file header
static HParser local_file_header_parser = h_sequence(
    signature_parser,
    uint16_parser,
    uint16_parser,
    uint16_parser,
    dos_time_parser,
    uint32_crc_parser,
    uint32_parser,
    uint32_parser,
    filename_len_parser,
    extra_field_len_parser,
    h_bytes_with_len(filename_len_parser),
    h_bytes_with_len(extra_field_len_parser)
);

// Parser for the central directory header (simplified)
static HParser central_directory_header_parser = h_sequence(
    signature_parser,
    uint16_parser,
    uint16_parser,
    uint16_parser,
    uint16_parser,
    dos_time_parser,
    uint32_crc_parser,
    uint32_parser,
    uint32_parser,
    filename_len_parser,
    extra_field_len_parser,
    file_comment_len_parser,
    uint16_parser,
    uint16_parser,
    uint32_parser,
    uint32_parser,
    h_bytes_with_len(filename_len_parser),
    h_bytes_with_len(extra_field_len_parser),
    h_bytes_with_len(file_comment_len_parser)
);

// Parser for the end of central directory record
static HParser end_of_central_directory_record_parser = h_sequence(
    signature_parser,
    uint16_parser,
    uint16_parser,
    uint16_parser,
    uint16_parser,
    uint32_parser,
    uint32_parser,
    file_comment_len_parser,
    h_bytes_with_len(file_comment_len_parser)
);

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <zip_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    HParseResult result;
    fseek(fp, -22, SEEK_END);
    uint8_t *buffer = malloc(22);
    fread(buffer, 1, 22, fp);
    result = h_parse(end_of_central_directory_record_parser, buffer, 22);
    if (h_result_is_err(result)) {
        fprintf(stderr, "Error parsing end of central directory record: %s\n", h_result_error(result));
        fclose(fp);
        free(buffer);
        return 1;
    }
    free(buffer);
    fclose(fp);
    return 0;
}
