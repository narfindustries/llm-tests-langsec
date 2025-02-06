#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *create_jpeg_parser() {
    // Define JPEG markers
    HParser *marker = h_choice(
        h_token("\xFF\xD8", 2), // SOI
        h_token("\xFF\xD9", 2), // EOI
        h_token("\xFF\xDB", 2), // DQT
        h_token("\xFF\xC0", 2), // SOF0
        h_token("\xFF\xC4", 2), // DHT
        h_token("\xFF\xDA", 2), // SOS
        h_token("\xFF\xDD", 2), // DRI
        h_choice(
            h_token("\xFF\xE0", 2), h_token("\xFF\xE1", 2),
            h_token("\xFF\xE2", 2), h_token("\xFF\xE3", 2),
            h_token("\xFF\xE4", 2), h_token("\xFF\xE5", 2),
            h_token("\xFF\xE6", 2), h_token("\xFF\xE7", 2),
            h_token("\xFF\xE8", 2), h_token("\xFF\xE9", 2),
            h_token("\xFF\xEA", 2), h_token("\xFF\xEB", 2),
            h_token("\xFF\xEC", 2), h_token("\xFF\xED", 2),
            h_token("\xFF\xEE", 2), h_token("\xFF\xEF", 2),
            NULL
        ), // APPn
        h_token("\xFF\xFE", 2), // COM
        h_choice(
            h_token("\xFF\xD0", 2), h_token("\xFF\xD1", 2),
            h_token("\xFF\xD2", 2), h_token("\xFF\xD3", 2),
            h_token("\xFF\xD4", 2), h_token("\xFF\xD5", 2),
            h_token("\xFF\xD6", 2), h_token("\xFF\xD7", 2),
            NULL
        ), // RSTn
        NULL
    );

    // Define length-prefixed segments
    HParser *length_prefixed_segment = h_sequence(
        marker,
        h_length_value(h_uint16(), h_uint8()),
        NULL
    );

    // Define JPEG structure
    HParser *jpeg_parser = h_sequence(
        h_token("\xFF\xD8", 2), // SOI
        h_many(length_prefixed_segment),
        h_token("\xFF\xD9", 2), // EOI
        NULL
    );

    return jpeg_parser;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <jpeg_file>\n", argv[0]);
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

    HParser *jpeg_parser = create_jpeg_parser();
    HParseResult *result = h_parse(jpeg_parser, data, file_size);

    if (result->ast) {
        printf("JPEG file parsed successfully.\n");
        h_ast_print(result->ast, stdout);
    } else {
        printf("Failed to parse JPEG file.\n");
    }

    h_parse_result_free(result);
    h_parser_free(jpeg_parser);
    free(data);

    return EXIT_SUCCESS;
}