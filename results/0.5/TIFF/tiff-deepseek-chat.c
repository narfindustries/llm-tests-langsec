#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define the TIFF header structure
HParser *tiff_header_parser() {
    return h_sequence(
        h_int16(), // Byte order (0x4949 for little-endian, 0x4D4D for big-endian)
        h_int16(), // Magic number (42)
        h_int32(), // Offset to the first IFD
        NULL
    );
}

// Define the IFD entry structure
HParser *ifd_entry_parser() {
    return h_sequence(
        h_int16(), // Tag
        h_int16(), // Type
        h_int32(), // Count
        h_int32(), // Value or offset to value
        NULL
    );
}

// Define the IFD structure
HParser *ifd_parser() {
    return h_sequence(
        h_int16(), // Number of directory entries
        h_many(ifd_entry_parser()), // Directory entries
        h_int32(), // Offset to the next IFD
        NULL
    );
}

// Define the TIFF parser
HParser *tiff_parser() {
    return h_sequence(
        tiff_header_parser(),
        h_many(ifd_parser()),
        NULL
    );
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <tiff_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = (uint8_t *)malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(tiff_parser(), data, file_size);
    if (!result) {
        fprintf(stderr, "Failed to parse TIFF file\n");
        free(data);
        return 1;
    }

    printf("TIFF file parsed successfully\n");

    h_parse_result_free(result);
    free(data);
    return 0;
}