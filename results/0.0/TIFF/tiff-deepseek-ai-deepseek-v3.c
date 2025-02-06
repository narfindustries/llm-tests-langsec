#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define TIFF header parser
HParser *tiff_header_parser() {
    return h_sequence(
        h_uint16(), // ByteOrder
        h_uint16(), // TIFF identifier
        h_uint32(), // Offset to first IFD
        NULL
    );
}

// Define IFD entry parser
HParser *ifd_entry_parser() {
    return h_sequence(
        h_uint16(), // Tag
        h_uint16(), // Type
        h_uint32(), // Count
        h_uint32(), // Value/Offset
        NULL
    );
}

// Define IFD parser
HParser *ifd_parser() {
    return h_sequence(
        h_uint16(), // Number of entries
        h_repeat_n(ifd_entry_parser(), h_length_value(h_uint16(), h_uint16())), // Entries
        h_uint32(), // Offset to next IFD
        NULL
    );
}

// Define TIFF parser
HParser *tiff_parser() {
    return h_sequence(
        tiff_header_parser(),
        h_repeat_n(ifd_parser(), h_length_value(h_uint32(), h_uint32())), // IFDs
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input file>\n", argv[0]);
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

    uint8_t *buffer = (uint8_t *)malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(tiff_parser(), buffer, file_size);
    if (!result) {
        fprintf(stderr, "Failed to parse TIFF file\n");
        free(buffer);
        return 1;
    }

    printf("Successfully parsed TIFF file\n");
    free(buffer);
    h_parse_result_free(result);
    return 0;
}