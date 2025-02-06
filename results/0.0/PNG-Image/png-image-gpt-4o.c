#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *create_png_parser() {
    // PNG Signature
    HParser *png_signature = h_sequence(
        h_uint8_val(0x89),
        h_uint8_val(0x50),
        h_uint8_val(0x4E),
        h_uint8_val(0x47),
        h_uint8_val(0x0D),
        h_uint8_val(0x0A),
        h_uint8_val(0x1A),
        h_uint8_val(0x0A),
        NULL
    );

    // Chunk Length
    HParser *chunk_length = h_uint32();

    // Chunk Type
    HParser *chunk_type = h_fixed_bytes(4);

    // IHDR Chunk Data
    HParser *ihdr_data = h_sequence(
        h_uint32(), // Width
        h_uint32(), // Height
        h_uint8(),  // Bit Depth
        h_uint8(),  // Color Type
        h_uint8(),  // Compression Method
        h_uint8(),  // Filter Method
        h_uint8(),  // Interlace Method
        NULL
    );

    // Generic Chunk Data
    HParser *chunk_data = h_length_value(chunk_length, h_uint8());

    // CRC
    HParser *crc = h_uint32();

    // Chunk
    HParser *chunk = h_sequence(
        chunk_length,
        chunk_type,
        h_choice(
            h_if(h_strncmp(chunk_type, "IHDR", 4) == 0, ihdr_data),
            chunk_data,
            NULL
        ),
        crc,
        NULL
    );

    // PNG File
    HParser *png_file = h_sequence(
        png_signature,
        h_many1(chunk),
        NULL
    );

    return png_file;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <png_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *png_parser = create_png_parser();
    HParseResult *result = h_parse(png_parser, data, file_size);

    if (result) {
        printf("PNG file parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse PNG file.\n");
    }

    free(data);
    h_parser_free(png_parser);

    return EXIT_SUCCESS;
}