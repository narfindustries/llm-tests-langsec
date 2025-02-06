#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Define PNG signature
HParser *png_signature() {
    return h_token("\x89\x50\x4E\x47\x0D\x0A\x1A\x0A", 8);
}

// Define IHDR chunk
HParser *ihdr_chunk() {
    return h_sequence(
        h_uint32(), // Length
        h_token("IHDR", 4),
        h_sequence(
            h_uint32(), // Width
            h_uint32(), // Height
            h_uint8(),  // Bit Depth
            h_uint8(),  // Color Type
            h_uint8(),  // Compression Method
            h_uint8(),  // Filter Method
            h_uint8()   // Interlace Method
        ),
        h_uint32(), // CRC
        NULL
    );
}

// Define PLTE chunk
HParser *plte_chunk() {
    return h_sequence(
        h_uint32(), // Length
        h_token("PLTE", 4),
        h_repeat(h_uint8(), h_uint32()), // Palette entries
        h_uint32(), // CRC
        NULL
    );
}

// Define IDAT chunk
HParser *idat_chunk() {
    return h_sequence(
        h_uint32(), // Length
        h_token("IDAT", 4),
        h_repeat(h_uint8(), h_uint32()), // Image data
        h_uint32(), // CRC
        NULL
    );
}

// Define IEND chunk
HParser *iend_chunk() {
    return h_sequence(
        h_uint32_val(0), // Length
        h_token("IEND", 4),
        h_uint32(), // CRC
        NULL
    );
}

// Define ancillary chunks (example: tEXt)
HParser *text_chunk() {
    return h_sequence(
        h_uint32(), // Length
        h_token("tEXt", 4),
        h_repeat(h_uint8(), h_uint32()), // Text data
        h_uint32(), // CRC
        NULL
    );
}

// Define a generic chunk parser
HParser *chunk_parser() {
    return h_choice(
        ihdr_chunk(),
        plte_chunk(),
        idat_chunk(),
        iend_chunk(),
        text_chunk(), // Add more ancillary chunks as needed
        NULL
    );
}

// Define PNG parser
HParser *png_parser() {
    return h_sequence(
        png_signature(),
        h_many(chunk_parser()),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <file.png>\n", argv[0]);
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

    unsigned char *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(png_parser(), buffer, file_size);
    if (result) {
        printf("PNG file parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse PNG file.\n");
    }

    free(buffer);
    return EXIT_SUCCESS;
}