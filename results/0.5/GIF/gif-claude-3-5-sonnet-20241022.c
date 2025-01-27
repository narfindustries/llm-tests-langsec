#include "hammer/hammer.h"
#include <stdio.h>

static const uint8_t EXTENSION_INTRODUCER = 0x21;
static const uint8_t IMAGE_SEPARATOR = 0x2C;
static const uint8_t TRAILER = 0x3B;

HParser* init_gif_parser() {
    // Header
    HParser* header = h_sequence(
        h_token((const uint8_t*)"GIF", 3),
        h_token((const uint8_t*)"89a", 3),
        NULL
    );

    // Logical Screen Descriptor
    HParser* screen_descriptor = h_sequence(
        h_uint16(), // width
        h_uint16(), // height
        h_bits(8, false), // packed field
        h_uint8(), // background color index
        h_uint8(), // pixel aspect ratio
        NULL
    );

    // Color Table
    HParser* color_entry = h_sequence(
        h_uint8(), // red
        h_uint8(), // green
        h_uint8(), // blue
        NULL
    );
    
    HParser* color_table = h_repeat_n(color_entry, 256);

    // Image Descriptor
    HParser* image_descriptor = h_sequence(
        h_ch(IMAGE_SEPARATOR),
        h_uint16(), // left position
        h_uint16(), // top position
        h_uint16(), // width
        h_uint16(), // height
        h_bits(8, false), // packed field
        NULL
    );

    // Extension
    HParser* extension = h_sequence(
        h_ch(EXTENSION_INTRODUCER),
        h_uint8(), // extension label
        h_length_value(h_uint8(), h_uint8()), // block size and data
        NULL
    );

    // Image Data
    HParser* image_data = h_sequence(
        h_uint8(), // LZW minimum code size
        h_many1(h_length_value(h_uint8(), h_uint8())), // blocks
        NULL
    );

    // Trailer
    HParser* trailer = h_ch(TRAILER);

    // Complete GIF format
    return h_sequence(
        header,
        screen_descriptor,
        h_optional(color_table),
        h_many(h_choice(extension, 
                       h_sequence(image_descriptor,
                                h_optional(color_table),
                                image_data,
                                NULL),
                       NULL)),
        trailer,
        NULL
    );
}