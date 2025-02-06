#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define the GZIP constants
#define ID1 0x1F
#define ID2 0x8B
#define CM_DEFLATE 0x08

// Bit masks for FLG (Flags)
#define FTEXT 0x01
#define FHCRC 0x02
#define FEXTRA 0x04
#define FNAME 0x08
#define FCOMMENT 0x10

// Parser for the GZIP header
HParser *gzip_header() {
    HParser *id1 = h_ch(ID1); // ID1
    HParser *id2 = h_ch(ID2); // ID2
    HParser *cm = h_ch(CM_DEFLATE); // Compression Method
    HParser *flg = h_uint8(); // Flags
    HParser *mtime = h_uint32(); // Modification Time
    HParser *xfl = h_uint8(); // Extra Flags
    HParser *os = h_uint8(); // Operating System

    return h_sequence(id1, id2, cm, flg, mtime, xfl, os, NULL);
}

// Parser for the optional extra field
HParser *gzip_extra_field() {
    HParser *xlen = h_uint16();
    HParser *xdata = h_repeat_n(h_uint8(), xlen);
    return h_sequence(xlen, xdata, NULL);
}

// Parser for the optional zero-terminated string
HParser *gzip_zstring() {
    return h_null();
}

// Parser for the GZIP file structure
HParser *gzip_parser() {
    HParser *header = gzip_header();
    HParser *extra = h_optional(gzip_extra_field());
    HParser *fname = h_optional(gzip_zstring());
    HParser *fcomment = h_optional(gzip_zstring());
    HParser *hcrc = h_optional(h_uint16());

    HParser *compressed_data = h_many1(h_uint8()); // Placeholder for actual compressed data parser
    HParser *crc32 = h_uint32();
    HParser *isize = h_uint32();

    return h_sequence(header, extra, fname, fcomment, hcrc, compressed_data, crc32, isize, NULL);
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gzip_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    // Seek to the end of the file to determine the file size
    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    fseek(file, 0, SEEK_SET);

    // Read the entire file into memory
    uint8_t *data = malloc(size);
    if (!data) {
        fprintf(stderr, "Failed to allocate memory\n");
        fclose(file);
        return 1;
    }

    if (fread(data, 1, size, file) != size) {
        fprintf(stderr, "Failed to read file\n");
        free(data);
        fclose(file);
        return 1;
    }

    fclose(file);

    // Create a GZIP parser and parse the data
    HParser *parser = gzip_parser();
    HParseResult *result = h_parse(parser, data, size);

    if (result) {
        printf("GZIP file parsed successfully.\n");
    } else {
        printf("Failed to parse GZIP file.\n");
    }

    // Cleanup
    h_parse_result_free(result);
    h_free_parser(parser);
    free(data);

    return 0;
}