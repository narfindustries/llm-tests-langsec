#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define TIFF field types
static HParser *uint8 = h_uint8();
static HParser *uint16 = h_uint16_le();
static HParser *uint32 = h_uint32_le();
static HParser *sint32 = h_int32_le();

// Define a parser for the IFD (Image File Directory) entry
static HParser *ifd_entry;

// Define a parser for the IFD
static HParser *ifd;

// Define a parser for the TIFF header
static HParser *tiff_header;

// Function to initialize parsers
void init_parsers() {
    ifd_entry = h_sequence(uint16, uint16, uint32, uint32, NULL);

    ifd = h_length_value(uint16, h_many(ifd_entry));

    tiff_header = h_sequence(uint16, uint16, uint32, h_indirect(ifd, NULL), NULL);
}

// Main function
int main(int argc, char **argv) {
    HParser *tiff_parser;
    HParseResult *result;
    HInputStream *input;
    size_t len;
    uint8_t *data;

    if (argc != 2) {
        fprintf(stderr, "Usage: %s <TIFF file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    len = ftell(file);
    fseek(file, 0, SEEK_SET);

    data = malloc(len);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    if (fread(data, 1, len, file) != len) {
        fprintf(stderr, "Failed to read file\n");
        free(data);
        fclose(file);
        return EXIT_FAILURE;
    }

    fclose(file);

    init_parsers();

    tiff_parser = tiff_header;

    input = h_input_stream_new(data, len);
    result = h_parse(tiff_parser, input);
    h_input_stream_close(input);

    if (result) {
        printf("Parse successful!\n");
        h_pprint(stdout, result->ast, 0, 0);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Parse failed!\n");
    }

    free(data);

    return result ? EXIT_SUCCESS : EXIT_FAILURE;
}