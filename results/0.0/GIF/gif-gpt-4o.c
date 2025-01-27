#include <hammer/hammer.h>

HParser *gif_parser() {
    // Define the GIF header
    HParser *header = h_sequence(
        h_ch('G'), h_ch('I'), h_ch('F'),
        h_choice(
            h_sequence(h_ch('8'), h_ch('7'), h_ch('a')),
            h_sequence(h_ch('8'), h_ch('9'), h_ch('a'))
        ),
        NULL
    );

    // Logical Screen Descriptor
    HParser *logical_screen_descriptor = h_sequence(
        h_uint16(), // Logical Screen Width
        h_uint16(), // Logical Screen Height
        h_bits(8),  // Packed Fields
        h_uint8(),  // Background Color Index
        h_uint8(),  // Pixel Aspect Ratio
        NULL
    );

    // Global Color Table
    HParser *global_color_table = h_repeat_n(h_uint8(), 3); // RGB triplets

    // Image Descriptor
    HParser *image_descriptor = h_sequence(
        h_ch(','),  // Image Separator
        h_uint16(), // Image Left Position
        h_uint16(), // Image Top Position
        h_uint16(), // Image Width
        h_uint16(), // Image Height
        h_bits(8),  // Packed Fields
        NULL
    );

    // Local Color Table
    HParser *local_color_table = h_repeat_n(h_uint8(), 3); // RGB triplets

    // LZW Minimum Code Size
    HParser *lzw_minimum_code_size = h_uint8();

    // Image Data
    HParser *image_data_block = h_sequence(
        h_uint8(), // Block Size
        h_repeat(h_uint8()) // Block Data
    );

    HParser *image_data = h_many1(image_data_block);

    // Trailer
    HParser *trailer = h_ch(';');

    // Full GIF Parser
    HParser *gif_parser = h_sequence(
        header,
        logical_screen_descriptor,
        h_optional(global_color_table),
        h_many(
            h_sequence(
                image_descriptor,
                h_optional(local_color_table),
                lzw_minimum_code_size,
                image_data
            )
        ),
        trailer,
        NULL
    );

    return gif_parser;
}