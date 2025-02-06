#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *marker_parser(uint16_t marker) {
    return h_sequence(h_octet((marker >> 8) & 0xFF), h_octet(marker & 0xFF), NULL);
}

HParser *parse_jpeg() {
    HParser *soi_parser = marker_parser(0xFFD8);

    HParser *app_parser = h_sequence(
        h_choice(marker_parser(0xFFE0), marker_parser(0xFFE1), NULL), 
        h_length_value(h_uint16(), h_many(h_uint8())),
        NULL
    );

    HParser *dqt_parser = h_sequence(marker_parser(0xFFDB), h_length_value(h_uint16(), h_many(h_uint8())), NULL);

    HParser *sof_parser = h_choice(
        marker_parser(0xFFC0), marker_parser(0xFFC1), marker_parser(0xFFC2), NULL
    );

    HParser *dht_parser = h_sequence(marker_parser(0xFFC4), h_length_value(h_uint16(), h_many(h_uint8())), NULL);

    HParser *sos_parser = h_sequence(marker_parser(0xFFDA), h_length_value(h_uint16(), h_many(h_uint8())), NULL);

    HParser *eoi_parser = marker_parser(0xFFD9);

    HParser *other_marker_parser = h_sequence(
        h_choice(marker_parser(0xFFFE), marker_parser(0xFFDD), NULL),
        h_length_value(h_uint16(), h_many(h_uint8())),
        NULL
    );

    HParser *jpeg_parser = h_sequence(
        soi_parser,
        h_many(
            h_choice(app_parser, dqt_parser, sof_parser, dht_parser, sos_parser, other_marker_parser, NULL)
        ),
        eoi_parser,
        NULL
    );

    return jpeg_parser;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <jpeg_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    const char *filename = argv[1];
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Cannot open file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long filesize = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *data = malloc(filesize);
    if (!data) {
        perror("Memory allocation failed");
        fclose(file);
        return EXIT_FAILURE;
    }

    if (fread(data, 1, filesize, file) != filesize) {
        perror("Failed to read file");
        free(data);
        fclose(file);
        return EXIT_FAILURE;
    }
    fclose(file);

    HParser *jpeg_parser = parse_jpeg();
    HParseResult *result = h_parse(jpeg_parser, data, filesize);

    if (result->ast) {
        printf("JPEG parsed successfully!\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse JPEG\n");
        h_parse_result_free(result);
    }

    h_parser_unref(jpeg_parser);
    free(data);
    return EXIT_SUCCESS;
}