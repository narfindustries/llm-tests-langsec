#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Forward declarations of parsers
static HParser *nitf_file;

// NITF field parsers
static HParser *field_FHDR;
static HParser *field_CLEVEL;
static HParser *field_STYPE;
static HParser *field_OSTAID;
static HParser *field_FDT;
static HParser *field_FTITLE;
static HParser *field_FSCLAS;
static HParser *field_FSCLSY;
static HParser *field_FSCODE;
static HParser *field_FSCTLH;
static HParser *field_FSREL;
static HParser *field_FSDCTP;
static HParser *field_FSDCDT;
static HParser *field_FSDCXM;
static HParser *field_FSORGN;
static HParser *field_FSCAUT;
static HParser *field_FSCTLN;
static HParser *field_FSCOP;
static HParser *field_FSCPYS;
static HParser *field_ENCRYP;
static HParser *field_FBKGC;
static HParser *field_ONAME;
static HParser *field_OPHONE;

void init_parsers() {
    field_FHDR = h_token("NITF", 4);
    field_CLEVEL = h_int_range(h_uint8(), 1, 99);
    field_STYPE = h_token("BF01", 4);
    field_OSTAID = h_repeat_n(h_uint8(), 10);
    field_FDT = h_repeat_n(h_uint8(), 14);
    field_FTITLE = h_repeat_n(h_uint8(), 80);
    field_FSCLAS = h_choice(h_ch('U'), h_ch('R'), h_ch('C'), h_ch('S'), h_ch('T'), NULL);
    field_FSCLSY = h_repeat_n(h_uint8(), 2);
    field_FSCODE = h_repeat_n(h_uint8(), 11);
    field_FSCTLH = h_repeat_n(h_uint8(), 2);
    field_FSREL = h_repeat_n(h_uint8(), 20);
    field_FSDCTP = h_repeat_n(h_uint8(), 2);
    field_FSDCDT = h_repeat_n(h_uint8(), 8);
    field_FSDCXM = h_repeat_n(h_uint8(), 4);
    field_FSORGN = h_repeat_n(h_uint8(), 27);
    field_FSCAUT = h_repeat_n(h_uint8(), 40);
    field_FSCTLN = h_repeat_n(h_uint8(), 15);
    field_FSCOP = h_uint16();
    field_FSCPYS = h_uint16();
    field_ENCRYP = h_uint8();
    field_FBKGC = h_repeat_n(h_uint8(), 3);
    field_ONAME = h_repeat_n(h_uint8(), 24);
    field_OPHONE = h_repeat_n(h_uint8(), 18);

    nitf_file = h_sequence(
        field_FHDR, field_CLEVEL, field_STYPE, field_OSTAID, field_FDT,
        field_FTITLE, field_FSCLAS, field_FSCLSY, field_FSCODE, field_FSCTLH,
        field_FSREL, field_FSDCTP, field_FSDCDT, field_FSDCXM, field_FSORGN,
        field_FSCAUT, field_FSCTLN, field_FSCOP, field_FSCPYS, field_ENCRYP,
        field_FBKGC, field_ONAME, field_OPHONE,
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
    size_t length = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *data = malloc(length);
    if (data) {
        fread(data, 1, length, fp);
        init_parsers();
        HParser *parser = nitf_file;
        HParseResult *result = h_parse(parser, data, length);
        if (result) {
            printf("Parse successful!\n");
            h_pprint(stdout, result->ast, 0, 4);
            h_parse_result_free(result);
        } else {
            printf("Parse failed!\n");
        }
        free(data);
    } else {
        fprintf(stderr, "Memory allocation failed\n");
    }

    fclose(fp);
    return 0;
}