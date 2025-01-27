#include <hammer/hammer.h>

// Define the GIF Header
HParser *gif_header(void) {
    return h_sequence(
        h_bytes("GIF", 3),            // Signature: "GIF"
        h_choice(
            h_bytes("87a", 3),        // Version: "87a"
            h_bytes("89a", 3)         // Version: "89a"
        ),
        NULL);
}

// Define the Logical Screen Descriptor
HParser *logical_screen_descriptor(void) {
    return h_sequence(
        h_uint16_le(),                // Logical Screen Width
        h_uint16_le(),                // Logical Screen Height
        h_bits(1),                    // Global Color Table Flag
        h_bits(3),                    // Color Resolution
        h_bits(1),                    // Sort Flag
        h_bits(3),                    // Size of Global Color Table
        h_uint8(),                    // Background Color Index
        h_uint8(),                    // Pixel Aspect Ratio
        NULL);
}

// Define the Color Table
HParser *color_table(int size) {
    return h_repeat_n(
        h_sequence(
            h_uint8(),                // Red
            h_uint8(),                // Green
            h_uint8()                 // Blue
        ),
        size * 3);                    // Each color is 3 bytes
}

// Define the Image Descriptor
HParser *image_descriptor(void) {
    return h_sequence(
        h_uint8_val(0x2C),            // Image Separator (0x2C)
        h_uint16_le(),                // Image Left Position
        h_uint16_le(),                // Image Top Position
        h_uint16_le(),                // Image Width
        h_uint16_le(),                // Image Height
        h_bits(1),                    // Local Color Table Flag
        h_bits(1),                    // Interlace Flag
        h_bits(1),                    // Sort Flag
        h_bits(2),                    // Reserved
        h_bits(3),                    // Size of Local Color Table
        NULL);
}

// Define the LZW Minimum Code Size
HParser *lzw_minimum_code_size(void) {
    return h_uint8();                 // LZW Minimum Code Size
}

// Define the Image Data
HParser *image_data(void) {
    return h_repeat(
        h_sequence(
            h_uint8(),                // Block Size
            h_bytes_p(h_uint8())      // Block Data
        ),
        h_uint8_val(0x00)             // Block Terminator
    );
}

// Define the Trailer
HParser *gif_trailer(void) {
    return h_uint8_val(0x3B);         // Trailer (0x3B)
}

// Define the complete GIF file parser
HParser *gif_file(void) {
    return h_sequence(
        gif_header(),
        logical_screen_descriptor(),
        h_optional(color_table(2 << h_bits_value(3))), // Global Color Table if present
        image_descriptor(),
        h_optional(color_table(2 << h_bits_value(3))), // Local Color Table if present
        lzw_minimum_code_size(),
        image_data(),
        gif_trailer(),
        NULL);
}

int main(int argc, char *argv[]) {
    // Initialize the parser
    HParser *parser = gif_file();
    
    // Use the parser on a GIF file
    // File reading and parsing logic would go here
    
    // Free the parser
    h_parser_free(parser);
    
    return 0;
}