#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

HParser *create_png_parser() {
    // PNG file signature: 8 bytes
    HParser *signature = h_sequence(
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

    // Length: 4 bytes
    HParser *length = h_uint32();

    // Chunk type: 4 bytes
    HParser *chunk_type = h_bytes(4);

    // Chunk data: variable length
    HParser *chunk_data = h_length_value(length, h_uint8());

    // CRC: 4 bytes
    HParser *crc = h_uint32();

    // PNG chunk: Length + Chunk Type + Chunk Data + CRC
    HParser *chunk = h_sequence(length, chunk_type, chunk_data, crc, NULL);

    // PNG file: Signature + chunks
    HParser *png_file = h_sequence(signature, h_many(chunk), NULL);

    return png_file;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <png-file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("fopen");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *data = malloc(file_size);
    if (!data) {
        perror("malloc");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *png_parser = create_png_parser();
    HParseResult *result = h_parse(png_parser, data, file_size);

    if (result->status == HParseSuccess) {
        printf("PNG file parsed successfully.\n");
    } else {
        printf("Failed to parse PNG file.\n");
    }

    h_parse_result_free(result);
    h_parser_free(png_parser);
    free(data);

    return EXIT_SUCCESS;
}