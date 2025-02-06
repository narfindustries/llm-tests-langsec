#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *chunk_length, *chunk_type, *chunk_data, *chunk_crc;
HParser *IHDR, *PLTE, *IDAT, *IEND, *png_file;

void setup_parsers() {
    // Define parsers for chunk components
    chunk_length = h_uint32();
    chunk_type = h_literal("IHDR", 4); // Example for IHDR, should be flexible in a real implementation
    chunk_crc = h_uint32();

    // IHDR Chunk
    IHDR = h_sequence(
        h_uint32(),   // Width
        h_uint32(),   // Height
        h_uint8(),    // Bit Depth
        h_uint8(),    // Color Type
        h_uint8(),    // Compression Method
        h_uint8(),    // Filter Method
        h_uint8(),    // Interlace Method
        NULL
    );

    // PLTE Chunk (Palette of RGB triplets)
    PLTE = h_sequence(
        chunk_length,
        h_literal("PLTE", 4),
        h_repeat_n(h_uint8(), 3), // RGB triplets
        chunk_crc,
        NULL
    );

    // IDAT Chunk (Compressed image data)
    IDAT = h_sequence(
        chunk_length,
        h_literal("IDAT", 4),
        h_repeat(h_uint8(), 1, h_n_times(-1)),  // Data of any length
        chunk_crc,
        NULL
    );

    // IEND Chunk
    IEND = h_sequence(
        h_literal("\x00\x00\x00\x00IEND\xAE\x42\x60\x82", 12), // Entire IEND chunk
        NULL
    );

    // PNG File parser
    png_file = h_sequence(
        h_literal("\x89PNG\r\n\x1A\n", 8), // PNG Signature
        h_many1(
            h_choice(
                h_sequence(chunk_length, h_literal("IHDR", 4), IHDR, chunk_crc, NULL),
                PLTE,
                IDAT,
                IEND,
                NULL
            )
        ),
        NULL
    );
}

void parse_png_file(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Error opening file");
        return;
    }

    fseek(file, 0, SEEK_END);
    size_t file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *data = (unsigned char *) malloc(file_size);
    if (!data) {
        perror("Memory allocation error");
        fclose(file);
        return;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(png_file, data, file_size);
    if (result) {
        printf("PNG file parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse PNG file.\n");
    }

    free(data);
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <png-file>\n", argv[0]);
        return 1;
    }

    setup_parsers();
    parse_png_file(argv[1]);

    return 0;
}