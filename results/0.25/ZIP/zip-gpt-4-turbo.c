#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Parser definitions for ZIP file format
HParser *zip_local_file_header() {
    return h_sequence(
        h_bits(32, false),
        h_uint16(), // version needed to extract
        h_uint16(), // general purpose bit flag
        h_uint16(), // compression method
        h_uint16(), // last mod file time
        h_uint16(), // last mod file date
        h_uint32(), // crc-32
        h_uint32(), // compressed size
        h_uint32(), // uncompressed size
        h_length_value(h_uint16(), h_bytes()), // file name
        h_length_value(h_uint16(), h_bytes()), // extra field
        NULL
    );
}

HParser *zip_central_directory_file_header() {
    return h_sequence(
        h_bits(32, false),
        h_uint16(), // version made by
        h_uint16(), // version needed to extract
        h_uint16(), // general purpose bit flag
        h_uint16(), // compression method
        h_uint16(), // last mod file time
        h_uint16(), // last mod file date
        h_uint32(), // crc-32
        h_uint32(), // compressed size
        h_uint32(), // uncompressed size
        h_length_value(h_uint16(), h_bytes()), // file name
        h_length_value(h_uint16(), h_bytes()), // extra field
        h_length_value(h_uint16(), h_bytes()), // file comment
        h_uint16(), // disk number start
        h_uint16(), // internal file attributes
        h_uint32(), // external file attributes
        h_uint32(), // relative offset of local header
        NULL
    );
}

HParser *zip_end_of_central_directory_record() {
    return h_sequence(
        h_bits(32, false),
        h_uint16(), // number of this disk
        h_uint16(), // disk where central directory starts
        h_uint16(), // number of central directory records on this disk
        h_uint16(), // total number of central directory records
        h_uint32(), // size of central directory
        h_uint32(), // offset of start of central directory
        h_length_value(h_uint16(), h_bytes()), // ZIP file comment
        NULL
    );
}

HParser *zip_file() {
    return h_sequence(
        h_many(zip_local_file_header()),
        h_many(zip_central_directory_file_header()),
        zip_end_of_central_directory_record(),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <zip_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if (fread(data, 1, size, file) != size) {
        perror("Failed to read file");
        free(data);
        fclose(file);
        return 1;
    }

    fclose(file);

    HParser *parser = zip_file();
    HParseResult *result = h_parse(parser, data, size);
    if (result) {
        printf("ZIP file parsed successfully.\n");
    } else {
        printf("Failed to parse ZIP file.\n");
    }

    h_parse_result_free(result);
    h_free_parser(parser);
    free(data);
    return 0;
}