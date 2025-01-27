#include <hammer/hammer.h>
#include <hammer/parsers.h>

const HParser* png_signature() {
    return h_sequence(
        h_literal("\x89PNG\r\n\x1a\n"),
        NULL
    );
}

const HParser* png_chunk_length() {
    return h_uint32();
}

const HParser* png_chunk_type() {
    return h_choice(
        h_literal("IHDR"),
        h_literal("IDAT"),
        h_literal("IEND"),
        NULL
    );
}

const HParser* png_chunk_data() {
    return h_many(h_int8());
}

const HParser* png_chunk_crc() {
    return h_uint32();
}

const HParser* png_chunk() {
    return h_sequence(
        png_chunk_length(),
        png_chunk_type(),
        png_chunk_data(),
        png_chunk_crc(),
        NULL
    );
}

const HParser* png_file() {
    return h_sequence(
        png_signature(),
        h_many(png_chunk()),
        NULL
    );
}

HParser* create_png_parser() {
    return h_build(png_file());
}