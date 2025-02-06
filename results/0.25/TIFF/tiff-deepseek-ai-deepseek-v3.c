#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define TIFF header parser
HParser *tiff_header_parser() {
    return h_sequence(h_int16(), h_int16(), h_int32(), NULL);
}

// Define IFD entry parser
HParser *ifd_entry_parser() {
    return h_sequence(h_int16(), h_int16(), h_int32(), h_int32(), NULL);
}

// Define IFD parser
HParser *ifd_parser() {
    return h_sequence(h_int16(), h_repeat_n(ifd_entry_parser(), h_uint16()), h_int32(), NULL);
}

// Define TIFF parser
HParser *tiff_parser() {
    return h_sequence(tiff_header_parser(), h_repeat_n(ifd_parser(), h_uint32()), NULL);
}

int main(int argc, char *argv[]) {
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

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(tiff_parser(), buffer, file_size);
    if (result) {
        printf("Successfully parsed TIFF file.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse TIFF file.\n");
    }

    free(buffer);
    return 0;
}