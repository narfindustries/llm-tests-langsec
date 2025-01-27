#include <hammer/hammer.h>
#include <hammer/glue.h>

// Forward declarations for recursive structures
static HParsedToken *act_identity(const HParseResult *p, void *user_data);

// GZIP file format specification
static HParser *gzip_file;

// Definitions of fields
static HParser *id1;
static HParser *id2;
static HParser *cm;
static HParser *flg;
static HParser *mtime;
static HParser *xfl;
static HParser *os;
static HParser *xlen;
static HParser *extra_fields;
static HParser *filename;
static HParser *comment;
static HParser *hcrc;
static HParser *compressed_data;
static HParser *crc32;
static HParser *isize;

// Helper parsers
static HParser *uint8;
static HParser *uint16le;
static HParser *uint32le;
static HParser *zlib_block;
static HParser *null_terminated_string;

// Actions
static HParsedToken *act_identity(const HParseResult *p, void *user_data) {
    return H_MAKE_BYTES(p->ast->token);
}

// Initialize parsers
void init_parsers() {
    uint8 = h_uint8();
    uint16le = h_le_u16();
    uint32le = h_le_u32();
    null_terminated_string = h_null_terminated(h_uint8(), 0);

    id1 = h_ch(0x1f);
    id2 = h_ch(0x8b);
    cm = h_ch(0x08);  // Compression method (deflate)
    flg = h_bits(8, false);
    mtime = uint32le;
    xfl = uint8;
    os = uint8;
    xlen = uint16le;
    extra_fields = h_length_value(xlen, h_bytes(xlen));
    filename = null_terminated_string;
    comment = null_terminated_string;
    hcrc = h_conditional(h_bits(1, false), uint16le, NULL);
    crc32 = uint32le;
    isize = uint32le;

    zlib_block = h_sequence(
        h_many(h_not(h_ch(0))),
        h_ch(0),
        NULL
    );

    compressed_data = zlib_block;

    gzip_file = h_sequence(
        id1,
        id2,
        cm,
        flg,
        mtime,
        xfl,
        os,
        h_optional(extra_fields),
        h_optional(filename),
        h_optional(comment),
        h_optional(hcrc),
        compressed_data,
        crc32,
        isize,
        NULL
    );
}

int main(int argc, char **argv) {
    HParser *parser;
    init_parsers();
    parser = gzip_file;

    // Parse input
    HParserResult *result = h_parse(parser, input, length);
    if (result->bit_length == -1) {
        printf("Parse failed!\n");
    } else {
        printf("Parse successful!\n");
    }

    h_parse_result_free(result);
    return 0;
}