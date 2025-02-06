#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

HParser *jpeg_parser() {
    HParser *marker = h_uint16();
    HParser *soi = h_sequence(h_uint16__m(NULL), h_uint16_eq(0xFFD8), NULL);
    HParser *eoi = h_sequence(h_uint16__m(NULL), h_uint16_eq(0xFFD9), NULL);
    HParser *appn = h_sequence(h_int_range(h_uint16__m(NULL), 0xFFE0, 0xFFEF), h_length_value(h_uint16(), h_uint8()), NULL);
    HParser *sof = h_sequence(h_int_range(h_uint16__m(NULL), 0xFFC0, 0xFFCF), h_length_value(h_uint16(), h_uint8()), NULL);
    HParser *dht = h_sequence(h_uint16__m(NULL), h_uint16_eq(0xFFC4), h_length_value(h_uint16(), h_uint8()), NULL);
    HParser *dqt = h_sequence(h_uint16__m(NULL), h_uint16_eq(0xFFDB), h_length_value(h_uint16(), h_uint8()), NULL);
    HParser *sos = h_sequence(h_uint16__m(NULL), h_uint16_eq(0xFFDA), h_length_value(h_uint16(), h_uint8()), NULL);
    HParser *dri = h_sequence(h_uint16__m(NULL), h_uint16_eq(0xFFDD), h_length_value(h_uint16(), h_uint8()), NULL);
    HParser *com = h_sequence(h_uint16__m(NULL), h_uint16_eq(0xFFFE), h_length_value(h_uint16(), h_uint8()), NULL);
    HParser *restart = h_sequence(h_int_range(h_uint16__m(NULL), 0xFFD0, 0xFFD7), NULL);
    HParser *segment = h_choice(soi, eoi, appn, sof, dht, dqt, sos, dri, com, restart, NULL);
    HParser *jpeg = h_sequence(soi, h_many1(segment), eoi, NULL);
    return jpeg;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <file>\n", argv[0]);
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

    uint8_t *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *parser = jpeg_parser();
    HParseResult *result = h_parse(parser, data, file_size);

    if (result) {
        printf("Parsing succeeded!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed.\n");
    }

    free(data);
    return 0;
}