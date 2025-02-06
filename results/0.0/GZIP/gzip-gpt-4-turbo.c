#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define parsers for each part of the GZIP format
static HParser *gzip_id, *gzip_cm, *gzip_flg, *gzip_mtime, *gzip_xfl, *gzip_os;
static HParser *gzip_xlen, *gzip_extra, *gzip_fname, *gzip_fcomment, *gzip_fhcrc;
static HParser *gzip_crc32, *gzip_isize;
static HParser *gzip_member;

void init_parsers() {
    gzip_id = h_sequence(h_int8(), h_int8(), NULL);
    gzip_cm = h_int8();
    gzip_flg = h_int8();
    gzip_mtime = h_int32();
    gzip_xfl = h_int8();
    gzip_os = h_int8();

    gzip_xlen = h_int16();
    gzip_extra = h_length_value(gzip_xlen, h_repeat_n(h_uint8(), h_get_uint16(gzip_xlen)));
    gzip_fname = h_sequence(h_many1(h_uint8()), h_int8(), NULL);
    gzip_fcomment = h_sequence(h_many1(h_uint8()), h_int8(), NULL);
    gzip_fhcrc = h_int16();

    gzip_crc32 = h_int32();
    gzip_isize = h_int32();

    HParser *optional_extra = h_optional(gzip_extra);
    HParser *optional_fname = h_optional(gzip_fname);
    HParser *optional_fcomment = h_optional(gzip_fcomment);
    HParser *optional_fhcrc = h_optional(gzip_fhcrc);

    gzip_member = h_sequence(
        gzip_id, gzip_cm, gzip_flg, gzip_mtime, gzip_xfl, gzip_os,
        optional_extra, optional_fname, optional_fcomment, optional_fhcrc,
        h_greedy(h_uint8()), // Compressed data (greedy)
        gzip_crc32, gzip_isize,
        NULL
    );
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gzip_file>\n", argv[0]);
        return 1;
    }

    init_parsers();

    // Open the GZIP file
    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("Failed to open file");
        return 1;
    }

    // Seek to the end to get the file size
    fseek(fp, 0, SEEK_END);
    size_t length = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    // Read the entire file into memory
    uint8_t *data = malloc(length);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(fp);
        return 1;
    }

    if (fread(data, 1, length, fp) != length) {
        perror("Failed to read file");
        free(data);
        fclose(fp);
        return 1;
    }

    fclose(fp);

    // Parse the GZIP data
    HParseResult *result = h_parse(gzip_member, data, length);
    if (result) {
        printf("Parse successful!\n");
        h_pprint(stdout, result->ast, 0, 4);
    } else {
        printf("Parse failed!\n");
    }

    free(data);
    return 0;
}