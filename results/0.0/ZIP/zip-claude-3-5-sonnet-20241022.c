#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// ZIP format parsers
HParser* create_local_file_header_parser() {
    return h_sequence(
        h_token((uint8_t*)"PK\x03\x04", 4),
        h_uint16(), // version
        h_uint16(), // flags
        h_uint16(), // compression
        h_uint16(), // mod time
        h_uint16(), // mod date
        h_uint32(), // crc-32
        h_uint32(), // compressed size
        h_uint32(), // uncompressed size
        h_uint16(), // filename length
        h_uint16(), // extra field length
        h_length_value(h_uint16(), h_uint8()), // filename
        h_length_value(h_uint16(), h_uint8())  // extra field
    );
}

HParser* create_central_directory_header_parser() {
    return h_sequence(
        h_token((uint8_t*)"PK\x01\x02", 4),
        h_uint16(), // version made by
        h_uint16(), // version needed
        h_uint16(), // flags
        h_uint16(), // compression
        h_uint16(), // mod time
        h_uint16(), // mod date
        h_uint32(), // crc-32
        h_uint32(), // compressed size
        h_uint32(), // uncompressed size
        h_uint16(), // filename length
        h_uint16(), // extra field length
        h_uint16(), // comment length
        h_uint16(), // disk number start
        h_uint16(), // internal attributes
        h_uint32(), // external attributes
        h_uint32(), // relative offset
        h_length_value(h_uint16(), h_uint8()), // filename
        h_length_value(h_uint16(), h_uint8()), // extra field
        h_length_value(h_uint16(), h_uint8())  // comment
    );
}

HParser* create_end_of_central_directory_parser() {
    return h_sequence(
        h_token((uint8_t*)"PK\x05\x06", 4),
        h_uint16(), // disk number
        h_uint16(), // disk with central directory
        h_uint16(), // disk entries
        h_uint16(), // total entries
        h_uint32(), // central directory size
        h_uint32(), // central directory offset
        h_length_value(h_uint16(), h_uint8()) // comment
    );
}

HParser* create_zip_parser() {
    return h_sequence(
        h_many(create_local_file_header_parser()),
        h_many(create_central_directory_header_parser()),
        create_end_of_central_directory_parser(),
        NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <zip_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* data = malloc(size);
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

    HParser* zip_parser = create_zip_parser();
    HParseResult* result = h_parse(zip_parser, data, size);

    if (result) {
        printf("Successfully parsed ZIP file\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse ZIP file\n");
    }

    free(data);
    fclose(file);
    return 0;
}