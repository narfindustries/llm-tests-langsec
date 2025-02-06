#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define parser for File Header (FHDR)
HParser *parse_fhdr() {
    return h_sequence(
        h_ch('N'), h_ch('I'), h_ch('T'), h_ch('F'), // FHDR
        h_int64(), // FVER
        h_int32(), // CLEVEL
        h_int32(), // STYPE
        h_length_value(h_uint16(), h_ascii_alnum()), // OSTAID
        h_length_value(h_uint16(), h_ascii_alnum()), // FDT
        h_length_value(h_uint16(), h_ascii_alnum()), // FTITLE
        h_length_value(h_uint16(), h_ascii_alnum()), // FSCLAS
        h_length_value(h_uint16(), h_ascii_alnum()), // FSCODE
        h_length_value(h_uint16(), h_ascii_alnum()), // FSCTLH
        h_length_value(h_uint16(), h_ascii_alnum()), // FSREL
        h_length_value(h_uint16(), h_ascii_alnum()), // FSDCTP
        h_length_value(h_uint16(), h_ascii_alnum()), // FSDCDT
        h_length_value(h_uint16(), h_ascii_alnum()), // FSDCXM
        h_length_value(h_uint16(), h_ascii_alnum()), // FSDG
        h_length_value(h_uint16(), h_ascii_alnum()), // FSDGTD
        h_length_value(h_uint16(), h_ascii_alnum()), // FSCLTX
        h_length_value(h_uint16(), h_ascii_alnum()), // FSCATP
        h_length_value(h_uint16(), h_ascii_alnum()), // FSCRSN
        h_length_value(h_uint16(), h_ascii_alnum()), // FSSRDT
        h_length_value(h_uint16(), h_ascii_alnum()), // FSCTLN
        h_length_value(h_uint16(), h_ascii_alnum()), // ONAME
        h_length_value(h_uint16(), h_ascii_alnum()), // OPHONE
        NULL
    );
}

// Define parser for Image Segment (IM)
HParser *parse_im() {
    return h_sequence(
        h_ch('I'), h_ch('M'), // IM
        h_length_value(h_uint16(), h_ascii_alnum()), // IDATIM
        NULL
    );
}

// Define parser for Graphic Segment (SX)
HParser *parse_sx() {
    return h_sequence(
        h_ch('S'), h_ch('X'), // SX
        h_length_value(h_uint16(), h_ascii_alnum()), // SD
        NULL
    );
}

// Define parser for Text Segment (TE)
HParser *parse_te() {
    return h_sequence(
        h_ch('T'), h_ch('E'), // TE
        h_length_value(h_uint16(), h_ascii_alnum()), // TEXT
        NULL
    );
}

// Define parser for Data Extension Segment (DE)
HParser *parse_de() {
    return h_sequence(
        h_ch('D'), h_ch('E'), // DE
        h_length_value(h_uint16(), h_ascii_alnum()), // DESDATA
        NULL
    );
}

// Define parser for Reserved Extension Segment (RE)
HParser *parse_re() {
    return h_sequence(
        h_ch('R'), h_ch('E'), // RE
        h_length_value(h_uint16(), h_ascii_alnum()), // REDATA
        NULL
    );
}

// Define parser for Security Tag (SECTAG)
HParser *parse_sectag() {
    return h_length_value(h_uint16(), h_ascii_alnum());
}

// Define parser for User Defined Header (UDHD)
HParser *parse_udhd() {
    return h_sequence(
        h_length_value(h_uint16(), h_ascii_alnum()), // UDHDL
        h_length_value(h_uint16(), h_ascii_alnum()), // UDHD
        NULL
    );
}

// Define parser for Extended Header (XHD)
HParser *parse_xhd() {
    return h_sequence(
        h_length_value(h_uint16(), h_ascii_alnum()), // XHDL
        h_length_value(h_uint16(), h_ascii_alnum()), // XHD
        NULL
    );
}

// Define parser for File Trailer (FT)
HParser *parse_ft() {
    return h_sequence(h_ch('F'), h_ch('T'), NULL);
}

// Main parser for NITF file
HParser *parse_nitf() {
    return h_sequence(
        parse_fhdr(),
        h_many(parse_im()),
        h_many(parse_sx()),
        h_many(parse_te()),
        h_many(parse_de()),
        h_many(parse_re()),
        h_many(parse_sectag()),
        parse_udhd(),
        parse_xhd(),
        parse_ft(),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if !file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = (uint8_t *)malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *parser = parse_nitf();
    HParseResult *result = h_parse(parser, data, file_size);

    if (result) {
        printf("Parsing successful!\n");
    } else {
        printf("Parsing failed!\n");
    }

    free(data);
    h_parse_result_free(result);
    return 0;
}