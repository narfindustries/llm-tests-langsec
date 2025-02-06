#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Initialize parsers for each field in the GZIP specification
HParser *p_id1() { return h_ch(0x1f); }
HParser *p_id2() { return h_ch(0x8b); }
HParser *p_cm() { return h_ch(0x08); }

HParser *p_flg() { return h_bits(8, false); }
HParser *p_mtime() { return h_int_le(32); }
HParser *p_xfl() { return h_bits(8, false); }
HParser *p_os() { return h_bits(8, false); }

HParser *p_xlen() { return h_int_le(16); }
HParser *p_extra() { return h_sequence(p_xlen(), h_data(h_length_value(p_xlen(), NULL)), NULL); }

HParser *p_fname() { return h_sequence(h_not(h_ch(0x00)), h_ch(0x00), NULL); }
HParser *p_comment() { return h_sequence(h_not(h_ch(0x00)), h_ch(0x00), NULL); }
HParser *p_hcrc() { return h_int_le(16); }

HParser *p_gzip_optional() {
    return h_sequence(
        h_optional(p_extra()),
        h_optional(p_fname()),
        h_optional(p_comment()),
        h_optional(p_hcrc()),
        NULL
    );
}

HParser *p_crc32() { return h_int_le(32); }
HParser *p_isize() { return h_int_le(32); }

HParser *p_gzip() {
    return h_sequence(
        p_id1(), p_id2(), p_cm(), p_flg(), p_mtime(), p_xfl(), p_os(),
        p_gzip_optional(),
        h_data(h_many1(h_any())),
        p_crc32(),
        p_isize(),
        NULL
    );
}

void parse_gzip_file(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        return;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    unsigned char *buffer = (unsigned char *)malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *gzip_parser = p_gzip();
    HParseResult *result = h_parse(gzip_parser, buffer, file_size);
    if (result) {
        printf("GZIP file parsed successfully!\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse GZIP file.\n");
    }

    h_parser_free(gzip_parser);
    free(buffer);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gzip_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    parse_gzip_file(argv[1]);
    return EXIT_SUCCESS;
}