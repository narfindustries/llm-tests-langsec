#include <hammer/hammer.h>

HParser *jpeg_marker() {
    return h_choice(
        h_ch(0xFF),
        h_ch_range(0xD0, 0xD7), // RST0-RST7
        h_ch(0xD8), // SOI
        h_ch(0xD9), // EOI
        h_ch(0xDA), // SOS
        h_ch(0xDB), // DQT
        h_ch(0xC0), // SOF0
        h_ch(0xC4), // DHT
        h_ch(0xDD), // DRI
        h_ch(0xE0), // APP0
        h_ch(0xFE), // COM
        NULL
    );
}

HParser *jpeg_segment() {
    return h_sequence(
        jpeg_marker(),
        h_uint16_be(),
        NULL
    );
}

HParser *jpeg_data() {
    return h_many1(h_uint8());
}

HParser *jpeg_parser() {
    return h_sequence(
        h_ch(0xFF),
        h_ch(0xD8), // SOI
        h_many(
            h_choice(
                jpeg_segment(),
                jpeg_data(),
                NULL
            )
        ),
        h_sequence(
            h_ch(0xFF),
            h_ch(0xD9), // EOI
            NULL
        ),
        NULL
    );
}

int main(int argc, char **argv) {
    HParser *parser = jpeg_parser();
    HParseResult *result;
    uint8_t *data;
    size_t length;
    FILE *file;

    if (argc != 2) {
        fprintf(stderr, "Usage: %s <jpeg-file>\n", argv[0]);
        return 1;
    }

    file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    length = ftell(file);
    fseek(file, 0, SEEK_SET);

    data = malloc(length);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if (fread(data, 1, length, file) != length) {
        perror("Failed to read file");
        free(data);
        fclose(file);
        return 1;
    }

    fclose(file);

    result = h_parse(parser, data, length);
    if (result) {
        printf("JPEG parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse JPEG.\n");
    }

    free(data);
    h_parser_free(parser);
    return 0;
}