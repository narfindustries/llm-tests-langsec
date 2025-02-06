#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *marker_parser(uint16_t value) {
    return h_sequence(h_uint8(), h_uint8(), NULL);
}

HParser *app_parser() {
    return h_choice(
        marker_parser(0xFFE0), // APP0 - JFIF
        marker_parser(0xFFE1), // APP1 - Exif
        NULL
    );
}

HParser *segment_length_parser() {
    return h_uint16();
}

HParser *dqt_parser() {
    return h_sequence(
        marker_parser(0xFFDB),
        segment_length_parser(),
        h_many(h_sequence(
            h_bits(4, false), // Precision
            h_bits(4, false), // Table identifier
            h_repeat_n(h_uint8(), 64) // Quantization table data
        )),
        NULL
    );
}

HParser *sof_parser() {
    return h_choice(
        marker_parser(0xFFC0), // Baseline DCT
        NULL
    );
}

HParser *dht_parser() {
    return h_sequence(
        marker_parser(0xFFC4),
        segment_length_parser(),
        h_many(h_sequence(
            h_bits(4, false), // Table class
            h_bits(4, false), // Table identifier
            h_repeat_n(h_uint8(), 16), // Number of symbols per bit length
            h_many(h_uint8()) // Symbols
        )),
        NULL
    );
}

HParser *sos_parser() {
    return h_sequence(
        marker_parser(0xFFDA),
        segment_length_parser(),
        h_bits(8, false), // Number of components in scan
        h_many(h_sequence(
            h_bits(8, false), // Component selector
            h_bits(4, false), // DC entropy coding table selector
            h_bits(4, false)  // AC entropy coding table selector
        )),
        h_bits(8, false), // Ss
        h_bits(8, false), // Se
        h_bits(4, false), // Ah
        h_bits(4, false), // Al
        NULL
    );
}

HParser *jpeg_parser() {
    return h_sequence(
        marker_parser(0xFFD8), // SOI
        h_many(h_choice(
            app_parser(),
            dqt_parser(),
            sof_parser(),
            dht_parser(),
            sos_parser(),
            marker_parser(0xFFDD), // DRI
            marker_parser(0xFFFE), // COM
            marker_parser(0xFFD0), // RST0
            NULL
        )),
        marker_parser(0xFFD9), // EOI
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <JPEG file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Unable to open file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *data = malloc(length);
    if (!data) {
        perror("Memory allocation failed");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(data, 1, length, file);
    fclose(file);

    HParser *parser = jpeg_parser();
    HParseResult *result = h_parse(parser, data, length);

    if (result && result->ast) {
        printf("JPEG file parsed successfully.\n");
    } else {
        printf("Failed to parse JPEG file.\n");
    }

    h_parse_result_free(result);
    free(data);
    return EXIT_SUCCESS;
}