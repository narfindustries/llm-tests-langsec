#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Function to read a file into memory
unsigned char *read_file(const char *filename, size_t *length) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    *length = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *data = (unsigned char *)malloc(*length);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return NULL;
    }

    fread(data, 1, *length, file);
    fclose(file);
    return data;
}

// Define parsers for the PNG file structure
HParser *png_signature_parser() {
    return h_sequence(
        h_uint8(), h_uint8(), h_uint8(), h_uint8(),
        h_uint8(), h_uint8(), h_uint8(), h_uint8(),
        NULL
    );
}

HParser *chunk_length_parser() {
    return h_uint32_be();
}

HParser *chunk_type_parser() {
    return h_bytes(4, 4);
}

HParser *chunk_data_parser(size_t length) {
    return h_repeat_n(h_uint8(), length);
}

HParser *chunk_crc_parser() {
    return h_uint32_be();
}

HParser *chunk_parser() {
    HParser *length = chunk_length_parser();
    HParser *type = chunk_type_parser();
    HParser *data = h_bind(length, (HContinuation)chunk_data_parser, NULL);
    HParser *crc = chunk_crc_parser();

    return h_sequence(length, type, data, crc, NULL);
}

HParser *png_parser() {
    return h_sequence(
        png_signature_parser(),
        h_many1(chunk_parser()),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input.png>\n", argv[0]);
        return EXIT_FAILURE;
    }

    size_t length;
    unsigned char *data = read_file(argv[1], &length);
    if (!data) {
        return EXIT_FAILURE;
    }

    HParser *parser = png_parser();
    HParseResult *result = h_parse(parser, data, length);

    if (result) {
        printf("PNG file parsed successfully!\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse PNG file.\n");
    }

    free(data);
    return EXIT_SUCCESS;
}