#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

// Helper function for null-terminated string
HParser *null_terminated_string() {
    return h_many1(h_until(h_uint8(), h_uint8_t(0x00)));
}

HParser *gzip_parser(void) {
    HParser *id1 = h_uint8_t(0x1F);
    HParser *id2 = h_uint8_t(0x8B);
    HParser *cm = h_uint8_t(0x08);
    HParser *flg = h_uint8();
    HParser *mtime = h_uint32();
    HParser *xfl = h_uint8();
    HParser *os = h_uint8();

    // Optional fields checked via flags
    HParser *extra = h_choice(
        h_sequence(h_uint16(), h_length_value(h_uint16(), h_uint8()), NULL),
        h_nothing(),
        NULL
    );

    HParser *fname = h_choice(
        null_terminated_string(),
        h_nothing(),
        NULL
    );

    HParser *fcomment = h_choice(
        null_terminated_string(),
        h_nothing(),
        NULL
    );

    HParser *fhcrc = h_choice(
        h_uint16(),
        h_nothing(),
        NULL
    );

    HParser *compressed_data = h_many(h_uint8());
    HParser *crc32 = h_uint32();
    HParser *isize = h_uint32();

    HParser *gzip_header = h_sequence(id1, id2, cm, flg, mtime, xfl, os, extra, fname, fcomment, fhcrc, NULL);
    HParser *gzip_footer = h_sequence(crc32, isize, NULL);

    return h_sequence(gzip_header, compressed_data, gzip_footer, NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gzip_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long filesize = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *filedata = malloc(filesize);
    if (!filedata) {
        perror("Memory allocation failed");
        fclose(file);
        return EXIT_FAILURE;
    }

    size_t read_bytes = fread(filedata, 1, filesize, file);
    if (read_bytes != filesize) {
        perror("Error reading file");
        free(filedata);
        fclose(file);
        return EXIT_FAILURE;
    }

    HParser *parser = gzip_parser();
    HParseResult *result = h_parse(parser, filedata, filesize);

    if (result) {
        printf("GZIP file parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse GZIP file.\n");
    }

    free(filedata);
    fclose(file);
    return EXIT_SUCCESS;
}