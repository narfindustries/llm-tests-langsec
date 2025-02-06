#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *tiff_parser();

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <tiff-file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *data = malloc(file_size);
    if (!data) {
        perror("Memory allocation failed");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *parser = tiff_parser();
    HParseResult *result = h_parse(parser, data, file_size);

    if (result) {
        printf("TIFF file parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse TIFF file.\n");
    }

    free(data);
    h_delete(parser);
    return EXIT_SUCCESS;
}

HParser *tiff_parser() {
    HParser *endian = h_choice(h_ch('I'), h_ch('M'), NULL);
    HParser *version = h_uint16();
    HParser *offset = h_uint32();

    HParser *ifd_entry = h_sequence(
        h_uint16(), // Tag
        h_uint16(), // Type
        h_uint32(), // Count
        h_uint32(), // Value/Offset
        NULL
    );

    HParser *ifd_entries = h_many(ifd_entry);
    HParser *ifd = h_sequence(
        h_uint16(), // Number of directory entries
        ifd_entries,
        h_uint32(), // Offset to next IFD
        NULL
    );

    HParser *tiff_header = h_sequence(
        endian,
        version,
        offset,
        NULL
    );

    HParser *tiff_file = h_sequence(
        tiff_header,
        ifd,
        NULL
    );

    return tiff_file;
}