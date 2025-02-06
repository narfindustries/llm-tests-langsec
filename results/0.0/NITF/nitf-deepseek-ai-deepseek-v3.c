#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define parsers for basic types
HParser *uint8_parser() { return h_uint8(); }
HParser *uint16_parser() { return h_uint16(); }
HParser *uint32_parser() { return h_uint32(); }
HParser *int32_parser() { return h_int32(); }
HParser *string_parser(size_t length) {
    return h_length_value(h_uint16(), h_repeat_n(h_uint8(), length));
}

// Define parsers for NITF fields
HParser *nitf_file_header_parser() {
    return h_sequence(
        h_token((uint8_t*)"NITF", 4), // FHDR
        string_parser(2), // VER
        uint8_parser(), // CLEVEL
        h_token((uint8_t*)"BF01", 4), // STYPE
        string_parser(10), // OSTAID
        string_parser(14), // FDT
        string_parser(80), // FTITLE
        string_parser(1), // FSCLAS
        string_parser(40), // FSCODE
        string_parser(40), // FSCTLH
        string_parser(40), // FSREL
        string_parser(2), // FSDCTP
        string_parser(8), // FSDCDT
        string_parser(4), // FSDCXM
        string_parser(1), // FSDG
        string_parser(8), // FSDGDT
        string_parser(43), // FSCLTX
        string_parser(1), // FSCATP
        string_parser(40), // FSCAUT
        string_parser(40), // FSCRSN
        string_parser(8), // FSSRDT
        string_parser(15), // FSCTLN
        uint8_parser(), // FSCOP
        uint8_parser(), // FSCPYS
        string_parser(27), // ONAME
        string_parser(24), // OPHONE
        NULL
    );
}

HParser *nitf_image_segment_header_parser() {
    return h_sequence(
        h_token((uint8_t*)"IM", 2), // IM
        string_parser(10), // IID
        string_parser(14), // IDATIM
        string_parser(17), // TGTID
        string_parser(80), // IID2
        string_parser(1), // ISCLAS
        string_parser(40), // ISCODE
        string_parser(40), // ISCTLH
        string_parser(40), // ISREL
        string_parser(2), // ISDCTP
        string_parser(8), // ISDCDT
        string_parser(4), // ISDCXM
        string_parser(1), // ISDG
        string_parser(8), // ISDGDT
        string_parser(43), // ISCLTX
        string_parser(1), // ISCATP
        string_parser(40), // ISCAUT
        string_parser(40), // ISCRSN
        string_parser(8), // ISSRDT
        string_parser(15), // ISCTLN
        uint8_parser(), // ENCRYP
        string_parser(42), // ISORCE
        uint32_parser(), // NROWS
        uint32_parser(), // NCOLS
        string_parser(3), // PVTYPE
        string_parser(8), // IREP
        string_parser(8), // ICAT
        uint8_parser(), // ABPP
        string_parser(1), // PJUST
        string_parser(1), // ICORDS
        string_parser(60), // IGEOLO
        uint8_parser(), // NICOM
        string_parser(80), // ICOM
        NULL
    );
}

HParser *nitf_graphic_segment_header_parser() {
    return h_sequence(
        h_token((uint8_t*)"GR", 2), // GR
        string_parser(10), // SID
        string_parser(20), // SNAME
        string_parser(1), // SSCLAS
        string_parser(40), // SSCODE
        string_parser(40), // SSCTLH
        string_parser(40), // SSREL
        string_parser(2), // SSDCTP
        string_parser(8), // SSDCDT
        string_parser(4), // SSDCXM
        string_parser(1), // SSDG
        string_parser(8), // SSDGDT
        string_parser(43), // SSCLTX
        string_parser(1), // SSCATP
        string_parser(40), // SSCAUT
        string_parser(40), // SSCRSN
        string_parser(8), // SSSRDT
        string_parser(15), // SSCTLN
        uint8_parser(), // ENCRYP
        string_parser(1), // SFMT
        string_parser(1), // SSTYP
        string_parser(14), // SIDATIM
        string_parser(20), // SNAME2
        string_parser(10), // SCOLOR
        int32_parser(), // SXORIG
        int32_parser(), // SYORIG
        int32_parser(), // SXEXT
        int32_parser(), // SYEXT
        NULL
    );
}

HParser *nitf_text_segment_header_parser() {
    return h_sequence(
        h_token((uint8_t*)"TE", 2), // TE
        string_parser(7), // TEXTID
        uint8_parser(), // TXTALVL
        string_parser(14), // TXTDT
        string_parser(80), // TXTITL
        string_parser(1), // TSCLAS
        string_parser(40), // TSCODE
        string_parser(40), // TSCTLH
        string_parser(40), // TSREL
        string_parser(2), // TSDCTP
        string_parser(8), // TSDCDT
        string_parser(4), // TSDCXM
        string_parser(1), // TSDG
        string_parser(8), // TSDGDT
        string_parser(43), // TSCLTX
        string_parser(1), // TSCATP
        string_parser(40), // TSCAUT
        string_parser(40), // TSCRSN
        string_parser(8), // TSSRDT
        string_parser(15), // TSCTLN
        uint8_parser(), // ENCRYP
        string_parser(3), // TXTFMT
        uint16_parser(), // TXSHDL
        uint32_parser(), // TXSOFL
        string_parser(80), // TXT
        NULL
    );
}

HParser *nitf_data_extension_segment_header_parser() {
    return h_sequence(
        h_token((uint8_t*)"DE", 2), // DE
        string_parser(25), // DESID
        string_parser(2), // DESVER
        uint32_parser(), // DESOFL
        string_parser(80), // DESITEM
        NULL
    );
}

HParser *nitf_reserved_extension_segment_header_parser() {
    return h_sequence(
        h_token((uint8_t*)"RE", 2), // RE
        string_parser(25), // RESID
        string_parser(2), // RESVER
        uint32_parser(), // RESOFL
        string_parser(80), // RESITEM
        NULL
    );
}

HParser *nitf_parser() {
    return h_sequence(
        nitf_file_header_parser(),
        h_repeat_n(nitf_image_segment_header_parser(), h_uint16()),
        h_repeat_n(nitf_graphic_segment_header_parser(), h_uint16()),
        h_repeat_n(nitf_text_segment_header_parser(), h_uint16()),
        h_repeat_n(nitf_data_extension_segment_header_parser(), h_uint16()),
        h_repeat_n(nitf_reserved_extension_segment_header_parser(), h_uint16()),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(nitf_parser(), buffer, file_size);
    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }

    free(buffer);
    return 0;
}