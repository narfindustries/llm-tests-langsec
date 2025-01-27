#include <hammer/hammer.h>

HParser *gif_parser() {
    // Define basic parsers
    HParser *header = h_sequence(
        h_ch('G'), h_ch('I'), h_ch('F'),
        h_choice(
            h_sequence(h_ch('8'), h_ch('7'), h_ch('a')),
            h_sequence(h_ch('8'), h_ch('9'), h_ch('a'))
        ),
        NULL
    );

    HParser *logical_screen_descriptor = h_sequence(
        h_uint16(), // Logical Screen Width
        h_uint16(), // Logical Screen Height
        h_bits(8),  // Packed Fields
        h_uint8(),  // Background Color Index
        h_uint8(),  // Pixel Aspect Ratio
        NULL
    );

    HParser *global_color_table = h_repeat_n(h_sequence(h_uint8(), h_uint8(), h_uint8(), NULL), 256);

    HParser *image_descriptor = h_sequence(
        h_ch(','),  // Image Separator
        h_uint16(), // Image Left Position
        h_uint16(), // Image Top Position
        h_uint16(), // Image Width
        h_uint16(), // Image Height
        h_bits(8),  // Packed Fields
        NULL
    );

    HParser *local_color_table = h_repeat_n(h_sequence(h_uint8(), h_uint8(), h_uint8(), NULL), 256);

    HParser *lzw_minimum_code_size = h_uint8();

    HParser *image_data = h_sequence(
        lzw_minimum_code_size,
        h_many(h_sequence(h_uint8(), h_data(h_uint8()))),
        h_ch(0), // Block Terminator
        NULL
    );

    HParser *graphic_control_extension = h_sequence(
        h_ch(0x21), h_ch(0xF9), h_ch(0x04),
        h_bits(8),  // Packed Fields
        h_uint16(), // Delay Time
        h_uint8(),  // Transparent Color Index
        h_ch(0),    // Block Terminator
        NULL
    );

    HParser *application_extension = h_sequence(
        h_ch(0x21), h_ch(0xFF), h_ch(0x0B),
        h_string("NETSCAPE2.0", 11),
        h_ch(0x03), h_ch(0x01),
        h_uint16(), // Loop Count
        h_ch(0),    // Block Terminator
        NULL
    );

    HParser *comment_extension = h_sequence(
        h_ch(0x21), h_ch(0xFE),
        h_many(h_sequence(h_uint8(), h_data(h_uint8()))),
        h_ch(0), // Block Terminator
        NULL
    );

    HParser *plain_text_extension = h_sequence(
        h_ch(0x21), h_ch(0x01),
        h_many(h_sequence(h_uint8(), h_data(h_uint8()))),
        h_ch(0), // Block Terminator
        NULL
    );

    HParser *trailer = h_ch(';');

    HParser *gif_file = h_sequence(
        header,
        logical_screen_descriptor,
        h_optional(global_color_table),
        h_many(h_choice(
            graphic_control_extension,
            comment_extension,
            application_extension,
            plain_text_extension,
            h_sequence(
                image_descriptor,
                h_optional(local_color_table),
                image_data,
                NULL
            )
        )),
        trailer,
        NULL
    );

    return gif_file;
}