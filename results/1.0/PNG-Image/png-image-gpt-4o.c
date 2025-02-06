#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *png_signature(void) {
    return h_sequence(
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
}

HParser *chunk_type(void) {
    return h_choice(
        h_token("IHDR", 4),
        h_token("PLTE", 4),
        h_token("IDAT", 4),
        h_token("IEND", 4),
        h_token("tEXt", 4),
        h_token("zTXt", 4),
        h_token("iTXt", 4),
        h_token("cHRM", 4),
        h_token("gAMA", 4),
        h_token("iCCP", 4),
        h_token("sBIT", 4),
        h_token("sRGB", 4),
        h_token("bKGD", 4),
        h_token("hIST", 4),
        h_token("tRNS", 4),
        h_token("pHYs", 4),
        h_token("sPLT", 4),
        h_token("tIME", 4),
        NULL
    );
}

HParser *ihdr_chunk(void) {
    return h_sequence(
        h_ignore(h_uint32()), // Length
        h_token("IHDR", 4),
        h_uint32(), // Width
        h_uint32(), // Height
        h_uint8(),  // Bit Depth
        h_uint8(),  // Color Type
        h_uint8_val(0x00), // Compression Method
        h_uint8_val(0x00), // Filter Method
        h_choice(h_uint8_val(0x00), h_uint8_val(0x01)), // Interlace Method
        h_ignore(h_uint32()),  // CRC
        NULL
    );
}

HParser *chunk(void) {
    return h_sequence(
        h_length_value(h_uint32(), 
            h_sequence(
                chunk_type(),
                h_data(h_uint32()), // Chunk Data
                h_ignore(h_uint32()), // CRC
                NULL
            )
        ),
        NULL
    );
}

HParser *png_file(void) {
    return h_sequence(
        png_signature(),
        ihdr_chunk(),
        h_many(chunk()),
        NULL
    );
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <filename>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *data = malloc(length);
    if (!data) {
        perror("Error allocating memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(data, 1, length, file);
    fclose(file);

    HParser *parser = png_file();
    HParseResult *result = h_parse(parser, data, length);

    if (result) {
        printf("PNG file parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse PNG file.\n");
    }

    h_parser_free(parser);
    free(data);

    return EXIT_SUCCESS;
}