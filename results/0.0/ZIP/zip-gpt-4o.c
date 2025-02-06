#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *create_zip_parser() {
    HParser *signature = h_bits(32, false);
    HParser *version = h_bits(16, false);
    HParser *bit_flag = h_bits(16, false);
    HParser *compression_method = h_bits(16, false);
    HParser *mod_time = h_bits(16, false);
    HParser *mod_date = h_bits(16, false);
    HParser *crc32 = h_bits(32, false);
    HParser *compressed_size = h_bits(32, false);
    HParser *uncompressed_size = h_bits(32, false);
    HParser *file_name_length = h_bits(16, false);
    HParser *extra_field_length = h_bits(16, false);
    HParser *file_name = h_length_value(file_name_length, h_uint8());
    HParser *extra_field = h_length_value(extra_field_length, h_uint8());

    HParser *local_file_header = h_sequence(
        h_token((const uint8_t *)"\x50\x4b\x03\x04", 4),
        version,
        bit_flag,
        compression_method,
        mod_time,
        mod_date,
        crc32,
        compressed_size,
        uncompressed_size,
        file_name_length,
        extra_field_length,
        file_name,
        extra_field,
        NULL
    );

    HParser *central_directory_file_header = h_sequence(
        h_token((const uint8_t *)"\x50\x4b\x01\x02", 4),
        version,
        version,
        bit_flag,
        compression_method,
        mod_time,
        mod_date,
        crc32,
        compressed_size,
        uncompressed_size,
        file_name_length,
        extra_field_length,
        h_bits(16, false), // file comment length
        h_bits(16, false), // disk number start
        h_bits(16, false), // internal file attributes
        h_bits(32, false), // external file attributes
        h_bits(32, false), // relative offset of local header
        file_name,
        extra_field,
        h_length_value(h_bits(16, false), h_uint8()), // file comment
        NULL
    );

    HParser *end_of_central_directory_record = h_sequence(
        h_token((const uint8_t *)"\x50\x4b\x05\x06", 4),
        h_bits(16, false), // number of this disk
        h_bits(16, false), // disk where central directory starts
        h_bits(16, false), // number of central directory records on this disk
        h_bits(16, false), // total number of central directory records
        h_bits(32, false), // size of central directory
        h_bits(32, false), // offset of start of central directory
        h_length_value(h_bits(16, false), h_uint8()), // zip file comment
        NULL
    );

    HParser *zip_parser = h_many1(h_choice(
        local_file_header,
        central_directory_file_header,
        end_of_central_directory_record,
        NULL
    ));

    return zip_parser;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <zipfile>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *zip_parser = create_zip_parser();
    HParseResult *result = h_parse(zip_parser, data, file_size);

    if (result) {
        printf("ZIP file parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse ZIP file.\n");
    }

    free(data);
    h_parser_free(zip_parser);

    return EXIT_SUCCESS;
}