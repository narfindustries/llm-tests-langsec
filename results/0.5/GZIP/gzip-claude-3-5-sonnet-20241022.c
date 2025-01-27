#include <hammer/hammer.h>
#include <stdio.h>

static HParser* init_gzip_parser(void) {
    // Header constants
    HParser* id1 = h_ch(0x1f);
    HParser* id2 = h_ch(0x8b);
    HParser* cm = h_ch(0x08);  // Deflate compression method

    // Flags
    HParser* ftext = h_bits(1, false);
    HParser* fhcrc = h_bits(1, false);
    HParser* fextra = h_bits(1, false);
    HParser* fname = h_bits(1, false);
    HParser* fcomment = h_bits(1, false);
    HParser* reserved = h_bits(3, false);
    HParser* flags = h_sequence(ftext, fhcrc, fextra, fname, fcomment, reserved, NULL);

    // Time, XFL, OS
    HParser* mtime = h_uint32();
    HParser* xfl = h_uint8();
    HParser* os = h_uint8();

    // Optional fields
    HParser* extra_length = h_optional(h_uint16());
    HParser* extra_field = h_optional(h_many(h_uint8()));
    HParser* filename = h_optional(h_many_until(h_uint8(), h_ch(0x00)));
    HParser* comment = h_optional(h_many_until(h_uint8(), h_ch(0x00)));
    HParser* crc16 = h_optional(h_uint16());

    // Compressed data and footer
    HParser* compressed_data = h_many(h_uint8());
    HParser* crc32 = h_uint32();
    HParser* isize = h_uint32();

    // Complete GZIP format
    return h_sequence(
        id1, id2, cm, flags,
        mtime, xfl, os,
        extra_length, extra_field,
        filename, comment, crc16,
        compressed_data,
        crc32, isize,
        NULL
    );
}

int main(int argc, char** argv) {
    HParser* parser = init_gzip_parser();
    if (!parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }

    // Parse input file
    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        fprintf(stderr, "Failed to open input file\n");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* input = malloc(size);
    if (!input) {
        fprintf(stderr, "Memory allocation failed\n");
        fclose(file);
        return 1;
    }

    if (fread(input, 1, size, file) != size) {
        fprintf(stderr, "Failed to read input file\n");
        free(input);
        fclose(file);
        return 1;
    }
    fclose(file);

    HParseResult* result = h_parse(parser, input, size);
    if (!result) {
        fprintf(stderr, "Parse failed\n");
        free(input);
        return 1;
    }

    // Cleanup
    h_parse_result_free(result);
    free(input);
    return 0;
}