#include <hammer/hammer.h>
#include <stdio.h>

static HParser* gif_signature() {
    return h_token((uint8_t*)"GIF87a", 6);
}

static HParser* screen_width() {
    return h_uint16();
}

static HParser* screen_height() {
    return h_uint16();
}

static HParser* packed_fields() {
    return h_bits(8, false);
}

static HParser* background_color() {
    return h_uint8();
}

static HParser* aspect_ratio() {
    return h_uint8();
}

static HParser* image_separator() {
    return h_ch(',');
}

static HParser* image_left() {
    return h_uint16();
}

static HParser* image_top() {
    return h_uint16();
}

static HParser* image_width() {
    return h_uint16();
}

static HParser* image_height() {
    return h_uint16();
}

static HParser* local_packed_fields() {
    return h_bits(8, false);
}

static HParser* lzw_min_code_size() {
    return h_uint8();
}

static HParser* block_size() {
    return h_uint8();
}

static HParser* image_data_block() {
    return h_length_value(block_size(), h_uint8());
}

static HParser* image_data() {
    return h_sequence(lzw_min_code_size(), h_many1(image_data_block()), h_ch('\0'), NULL);
}

static HParser* trailer() {
    return h_ch(';');
}

static HParser* gif_image() {
    return h_sequence(
        image_separator(),
        image_left(),
        image_top(),
        image_width(),
        image_height(),
        local_packed_fields(),
        image_data(),
        NULL
    );
}

static HParser* gif_parser() {
    return h_sequence(
        gif_signature(),
        screen_width(),
        screen_height(),
        packed_fields(),
        background_color(),
        aspect_ratio(),
        h_many1(gif_image()),
        trailer(),
        NULL
    );
}

H_RULE(gif_format, gif_parser());