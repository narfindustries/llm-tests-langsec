#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// Parser for Local File Header
HParser* local_file_header() {
    return h_sequence(
        h_token((uint8_t*)"\x50\x4b\x03\x04", 4),  // signature
        h_uint16(),                                 // version needed
        h_uint16(),                                 // general purpose flag
        h_uint16(),                                 // compression method
        h_uint16(),                                 // last mod time
        h_uint16(),                                 // last mod date
        h_uint32(),                                 // crc-32
        h_uint32(),                                 // compressed size
        h_uint32(),                                 // uncompressed size
        h_length_value(h_uint16(), h_uint8()),     // filename
        h_length_value(h_uint16(), h_uint8()),     // extra field
        NULL);
}

// Parser for Data Descriptor (optional)
HParser* data_descriptor() {
    return h_sequence(
        h_token((uint8_t*)"\x50\x4b\x07\x08", 4),  // signature
        h_uint32(),                                 // crc-32
        h_uint32(),                                 // compressed size
        h_uint32(),                                 // uncompressed size
        NULL);
}

// Parser for Central Directory Header
HParser* central_directory_header() {
    return h_sequence(
        h_token((uint8_t*)"\x50\x4b\x01\x02", 4),  // signature
        h_uint16(),                                 // version made by
        h_uint16(),                                 // version needed
        h_uint16(),                                 // general purpose flag
        h_uint16(),                                 // compression method
        h_uint16(),                                 // last mod time
        h_uint16(),                                 // last mod date
        h_uint32(),                                 // crc-32
        h_uint32(),                                 // compressed size
        h_uint32(),                                 // uncompressed size
        h_length_value(h_uint16(), h_uint8()),     // filename
        h_length_value(h_uint16(), h_uint8()),     // extra field
        h_length_value(h_uint16(), h_uint8()),     // file comment
        h_uint16(),                                 // disk number start
        h_uint16(),                                 // internal attrs
        h_uint32(),                                 // external attrs
        h_uint32(),                                 // local header offset
        NULL);
}

// Parser for End of Central Directory Record
HParser* end_of_central_directory() {
    return h_sequence(
        h_token((uint8_t*)"\x50\x4b\x05\x06", 4),  // signature
        h_uint16(),                                 // disk number
        h_uint16(),                                 // disk with CD
        h_uint16(),                                 // disk entries
        h_uint16(),                                 // total entries
        h_uint32(),                                 // CD size
        h_uint32(),                                 // CD offset
        h_length_value(h_uint16(), h_uint8()),     // comment
        NULL);
}

// Parser for ZIP64 End of Central Directory Record
HParser* zip64_end_of_central_directory() {
    return h_sequence(
        h_token((uint8_t*)"\x50\x4b\x06\x06", 4),  // signature
        h_uint64(),                                 // size of record
        h_uint16(),                                 // version made by
        h_uint16(),                                 // version needed
        h_uint32(),                                 // disk number
        h_uint32(),                                 // disk with CD
        h_uint64(),                                 // disk entries
        h_uint64(),                                 // total entries
        h_uint64(),                                 // CD size
        h_uint64(),                                 // CD offset
        NULL);
}

// Parser for ZIP64 End of Central Directory Locator
HParser* zip64_end_of_central_directory_locator() {
    return h_sequence(
        h_token((uint8_t*)"\x50\x4b\x06\x07", 4),  // signature
        h_uint32(),                                 // disk with zip64 EOCD
        h_uint64(),                                 // zip64 EOCD offset
        h_uint32(),                                 // total disks
        NULL);
}

// Main ZIP parser
HParser* zip_parser() {
    return h_sequence(
        h_many(h_sequence(
            local_file_header(),
            h_many(h_uint8()),                      // file data
            h_optional(data_descriptor()),
            NULL)),
        h_many1(central_directory_header()),
        h_optional(zip64_end_of_central_directory()),
        h_optional(zip64_end_of_central_directory_locator()),
        end_of_central_directory(),
        NULL);
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <zip_file>\n", argv[0]);
        return 1;
    }

    FILE* f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t* input = malloc(size);
    if (!input) {
        perror("Failed to allocate memory");
        fclose(f);
        return 1;
    }

    if (fread(input, 1, size, f) != size) {
        perror("Failed to read file");
        free(input);
        fclose(f);
        return 1;
    }
    fclose(f);

    HParser* parser = zip_parser();
    HParseResult* result = h_parse(parser, input, size);

    if (result) {
        printf("Successfully parsed ZIP file\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse ZIP file\n");
    }

    free(input);
    return result ? 0 : 1;
}