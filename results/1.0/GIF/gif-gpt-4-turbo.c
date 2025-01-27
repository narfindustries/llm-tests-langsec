#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Helper functions for creating parsers
static HParser *h_uint16_le() {
    return h_le_u16();
}

static HParser *h_uint32_le() {
    return h_le_u32();
}

// Header parser
static HParser *gif_header() {
    return h_sequence(h_string("GIF", 3), h_ch_range('7', '9'), h_ch('a'), NULL);
}

// Logical Screen Descriptor parser
static HParser *logical_screen_descriptor() {
    return h_sequence(
        h_uint16_le(),  // Width
        h_uint16_le(),  // Height
        h_bits(1, false),  // Global Color Table Flag
        h_bits(3, false),  // Color Resolution
        h_bits(1, false),  // Sorted
        h_bits(3, false),  // Size of Global Color Table
        h_uint8(),         // Background Color Index
        h_uint8(),         // Pixel Aspect Ratio
        NULL
    );
}

// Color table parser
static HParser *color_table(int size) {
    return h_repeat_n(h_sequence(h_uint8(), h_uint8(), h_uint8(), NULL), size);
}

// Graphics Control Extension parser
static HParser *graphics_control_extension() {
    return h_sequence(
        h_uint8(),  // Block Size
        h_bits(3, false),  // Reserved
        h_bits(3, false),  // Disposal Method
        h_bits(1, false),  // User Input Flag
        h_bits(1, false),  // Transparent Color Flag
        h_uint16_le(),     // Delay Time
        h_uint8(),         // Transparent Color Index
        h_uint8(),         // Block Terminator
        NULL
    );
}

// Image Descriptor parser
static HParser *image_descriptor() {
    return h_sequence(
        h_uint16_le(),  // Left Position
        h_uint16_le(),  // Top Position
        h_uint16_le(),  // Width
        h_uint16_le(),  // Height
        h_bits(1, false),  // Local Color Table Flag
        h_bits(1, false),  // Interlace Flag
        h_bits(1, false),  // Sorted
        h_bits(2, false),  // Reserved
        h_bits(3, false),  // Size of Local Color Table
        NULL
    );
}

// Image Data parser
static HParser *image_data() {
    return h_sequence(
        h_uint8(), // LZW Minimum Code Size
        h_indirect(),  // Data Blocks
        NULL
    );
}

// Block parser for different sections
static HParser *block() {
    return h_choice(
        graphics_control_extension(),
        h_sequence(h_ch(','), image_descriptor(), image_data(), NULL),  // Image Block
        h_sequence(h_ch('!'), h_uint8(), h_many(h_uint8()), h_uint8(), NULL),  // Extension Block
        NULL
    );
}

// Full GIF parser
static HParser *gif_parser() {
    return h_sequence(
        gif_header(),
        logical_screen_descriptor(),
        h_optional(color_table(256)),  // Optional Global Color Table 
        h_many(block()),              // Multiple Blocks
        h_ch(';'),                    // Trailer
        NULL
    );
}

int main(int argc, char **argv) {
    HParser *parser = gif_parser();
    // To be used with Hammer parser functions, e.g., h_parse(), that are not shown here
    return EXIT_SUCCESS;
}