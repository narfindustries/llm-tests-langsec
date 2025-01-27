#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// ZIP format parsers
HParser* make_zip_parser() {
    // Basic integer parsers
    HParser* uint16 = h_uint16();
    HParser* uint32 = h_uint32();

    // Local file header signature (0x04034b50)
    HParser* local_file_header_sig = h_token((uint8_t*)"\x50\x4b\x03\x04", 4);

    // Central directory header signature (0x02014b50)
    HParser* central_dir_sig = h_token((uint8_t*)"\x50\x4b\x01\x02", 4);

    // End of central directory signature (0x06054b50)
    HParser* end_central_dir_sig = h_token((uint8_t*)"\x50\x4b\x05\x06", 4);

    // File name and extra field parsers
    HParser* filename = h_length_value(h_uint16(), h_uint8());
    HParser* extra_field = h_length_value(h_uint16(), h_uint8());
    HParser* file_comment = h_length_value(h_uint16(), h_uint8());

    // Local file header
    HParser* local_file_header = h_sequence(
        local_file_header_sig,
        uint16,                    // version needed to extract
        uint16,                    // general purpose bit flag
        uint16,                    // compression method
        uint16,                    // last mod file time
        uint16,                    // last mod file date
        uint32,                    // crc-32
        uint32,                    // compressed size
        uint32,                    // uncompressed size
        filename,
        extra_field,
        NULL
    );

    // Central directory structure
    HParser* central_dir_header = h_sequence(
        central_dir_sig,
        uint16,                    // version made by
        uint16,                    // version needed to extract
        uint16,                    // general purpose bit flag
        uint16,                    // compression method
        uint16,                    // last mod file time
        uint16,                    // last mod file date
        uint32,                    // crc-32
        uint32,                    // compressed size
        uint32,                    // uncompressed size
        filename,
        extra_field,
        file_comment,
        uint16,                    // disk number start
        uint16,                    // internal file attributes
        uint32,                    // external file attributes
        uint32,                    // relative offset of local header
        NULL
    );

    // End of central directory record
    HParser* end_central_dir = h_sequence(
        end_central_dir_sig,
        uint16,                    // number of this disk
        uint16,                    // disk where central directory starts
        uint16,                    // number of central directory records on this disk
        uint16,                    // total number of central directory records
        uint32,                    // size of central directory
        uint32,                    // offset of start of central directory
        file_comment,
        NULL
    );

    // Complete ZIP file format
    return h_sequence(
        h_many(local_file_header),
        h_many(central_dir_header),
        end_central_dir,
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <zip_file>\n", argv[0]);
        return 1;
    }

    // Read input file
    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t *input = malloc(size);
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

    // Parse ZIP file
    HParser *zip_parser = make_zip_parser();
    HParseResult *result = h_parse(zip_parser, input, size);

    if (result) {
        printf("Successfully parsed ZIP file\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse ZIP file\n");
    }

    free(input);
    return 0;
}