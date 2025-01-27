#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define the GIF file structure parser
static HParser* gif_parser(void) {
    // Magic number identifier
    HParser* magic = h_literal_string("GIF87a");

    // Logical Screen Descriptor
    HParser* width = h_uint16();
    HParser* height = h_uint16();
    HParser* packed_fields = h_uint8();
    HParser* background_color_index = h_uint8();
    HParser* pixel_aspect_ratio = h_uint8();

    // Combine logical screen descriptor elements
    HParser* logical_screen_descriptor = h_sequence(
        width, height, 
        packed_fields, 
        background_color_index, 
        pixel_aspect_ratio, 
        NULL
    );

    // Global Color Table (optional)
    HParser* color_table_flag = h_bits(1, false);
    HParser* color_resolution = h_bits(3, false);
    HParser* sort_flag = h_bits(1, false);
    HParser* global_color_table_size = h_bits(3, false);

    // Full GIF file structure
    HParser* gif_file = h_sequence(
        magic,
        logical_screen_descriptor,
        NULL
    );

    return gif_file;
}

int main(int argc, char** argv) {
    // Initialize Hammer parser
    HParser* parser = gif_parser();

    // Input handling
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <gif_file>\n", argv[0]);
        return 1;
    }

    // Read file contents
    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    // Get file size
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    // Allocate buffer
    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    // Read file contents
    size_t read_size = fread(buffer, 1, file_size, file);
    fclose(file);

    if (read_size != file_size) {
        perror("File read error");
        free(buffer);
        return 1;
    }

    // Parse GIF file
    HParseResult* result = h_parse(parser, buffer, read_size);

    // Check parsing result
    if (result && result->ast) {
        printf("GIF file parsed successfully\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Parsing failed\n");
    }

    // Cleanup
    free(buffer);
    h_parser_free(parser);

    return 0;
}