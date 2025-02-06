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

HParser *parse_field_generic(int length) {
    return h_repeat_n(h_uint8(), length);
}

HParser *parse_nitf_header() {
    return h_sequence(
        parse_fhdr(),
        parse_clevel(),
        parse_stype(),
        parse_ostaid(),
        parse_fdt(),
        parse_fsclas(),
        parse_field_generic(2), // FSCLSY
        parse_field_generic(11), // FSCODE
        parse_field_generic(2), // FSCTLH
        parse_field_generic(20), // FSREL
        parse_field_generic(2), // FSDCTP
        parse_field_generic(8), // FSDCDT
        parse_field_generic(4), // FSDCXM
        parse_field_generic(1), // FSDG
        parse_field_generic(8), // FSDGDT
        parse_field_generic(43), // FSCLTX
        parse_field_generic(1), // FSCATP
        parse_field_generic(40), // FSCAUT
        parse_field_generic(1), // FSCRSN
        parse_field_generic(8), // FSSRDT
        parse_field_generic(15), // FSCTLN
        parse_field_generic(5), // FSCOP
        parse_field_generic(5), // FSCPYS
        parse_field_generic(1), // ENCRYP
        parse_field_generic(3), // FBKGC
        parse_field_generic(24), // ONAME
        parse_field_generic(18), // OPHONE
        NULL
    );
}

// Main function to parse a NITF file
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
    size_t filesize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buffer = malloc(filesize);
    if (!buffer) {
        perror("Memory allocation failed");
        fclose(fp);
        return EXIT_FAILURE;
    }

    fread(buffer, 1, filesize, fp);
    fclose(fp);

    HParser *nitf_parser = parse_nitf_header();
    HParseResult *result = h_parse(nitf_parser, buffer, filesize);

    if (result) {
        printf("NITF file parsed successfully.\n");
    } else {
        printf("Failed to parse NITF file.\n");
    }

    h_parse_result_free(result);
    h_parser_unref(nitf_parser);
    free(buffer);

    return 0;
}