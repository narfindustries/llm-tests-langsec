#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <string.h>

static HParser *gzip_parser;

static HParseResult* parse_gzip(void* data, size_t len) {
    return h_parse(gzip_parser, data, len);
}

static HParsedToken* reduce_header(const HParseResult* p, void* user_data) {
    HParsedToken* header = h_make_token(TT_SEQUENCE, p->ast);
    return header;
}

static HParsedToken* reduce_compressed_data(const HParseResult* p, void* user_data) {
    HParsedToken* data = h_make_token(TT_SEQUENCE, p->ast);
    return data;
}

static HParsedToken* reduce_footer(const HParseResult* p, void* user_data) {
    HParsedToken* footer = h_make_token(TT_SEQUENCE, p->ast);
    return footer;
}

void init_gzip_parser() {
    HParser* magic_number = h_sequence(
        h_ch(0x1F),
        h_ch(0x8B),
        NULL
    );

    HParser* compression_method = h_ch(8);  // Deflate method
    HParser* flags = h_bits(8, false);
    HParser* mtime = h_bits(32, false);
    HParser* extra_flags = h_bits(8, false);
    HParser* os_type = h_bits(8, false);

    HParser* header = h_sequence(
        magic_number,
        compression_method,
        flags,
        mtime,
        extra_flags,
        os_type,
        NULL
    );

    HParser* compressed_data = h_many(h_bits(8, false));

    HParser* crc32 = h_bits(32, false);
    HParser* original_size = h_bits(32, false);

    HParser* footer = h_sequence(
        crc32,
        original_size,
        NULL
    );

    gzip_parser = h_sequence(
        h_action(header, reduce_header),
        h_action(compressed_data, reduce_compressed_data),
        h_action(footer, reduce_footer),
        NULL
    );
}

int main(int argc, char** argv) {
    init_gzip_parser();
    return 0;
}