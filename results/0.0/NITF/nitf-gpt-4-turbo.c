#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define parsers for various fields
HParser *parse_fhdr() {
    return h_sequence(h_ch_range('0', '9'), h_ch_range('0', '9'), h_ch_range('0', '9'), h_ch_range('0', '9'), h_ch_range('0', '9'), h_ch_range('0', '9'), NULL);
}

HParser *parse_clevel() {
    return h_int_range(h_uint8(), 1, 99);
}

HParser *parse_stype() {
    return h_token("BF01", 4);
}

HParser *parse_ostaid() {
    return h_token("ABCDEFGHIJ", 10);
}

HParser *parse_fdt() {
    return h_sequence(h_ch_range('0', '9'), h_ch_range('0', '9'), h_ch_range('0', '9'), h_ch_range('0', '9'), h_ch_range('0', '9'), h_ch_range('0', '9'), h_ch_range('0', '9'), h_ch_range('0', '9'), h_ch_range('0', '9'), h_ch_range('0', '9'), h_ch_range('0', '9'), h_ch_range('0', '9'), h_ch_range('0', '9'), h_ch_range('0', '9'), NULL);
}

HParser *parse_fsclas() {
    return h_choice(h_ch('U'), h_ch('R'), h_ch('C'), h_ch('S'), h_ch('T'), NULL);
}

HParser *parse_encrypt() {
    return h_int_range(h_uint8(), 0, 1);
}

HParser *parse_fbkgc() {
    return h_token("000", 3);
}

HParser *parse_oname() {
    return h_token("ABCDEFGHIJ", 10);
}

HParser *parse_ophone() {
    return h_sequence(h_ch_range('0', '9'), h_ch_range('0', '9'), h_ch_range('0', '9'), h_ch_range('0', '9'), h_ch_range('0', '9'), h_ch_range('0', '9'), h_ch_range('0', '9'), h_ch_range('0', '9'), h_ch_range('0', '9'), h_ch_range('0', '9'), NULL);
}

// Main parser for NITF
HParser *nitf_parser() {
    return h_sequence(
        parse_fhdr(),
        parse_clevel(),
        parse_stype(),
        parse_ostaid(),
        parse_fdt(),
        parse_fsclas(),
        parse_encrypt(),
        parse_fbkgc(),
        parse_oname(),
        parse_ophone(),
        NULL
    );
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <nitf_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("File opening failed");
        return EXIT_FAILURE;
    }

    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *data = malloc(size);
    if (!data) {
        perror("Memory allocation failed");
        fclose(fp);
        return EXIT_FAILURE;
    }

    fread(data, 1, size, fp);
    fclose(fp);

    HParser *parser = nitf_parser();
    HParseResult *result = h_parse(parser, data, size);
    if (result) {
        printf("Parse successful!\n");
    } else {
        printf("Parse failed!\n");
    }

    h_parse_result_free(result);
    free(data);
    return 0;
}