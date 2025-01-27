#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define JPEG markers
#define SOI  0xFFD8
#define APP0 0xFFE0
#define APP1 0xFFE1
#define APP2 0xFFE2
#define APP3 0xFFE3
#define APP4 0xFFE4
#define APP5 0xFFE5
#define APP6 0xFFE6
#define APP7 0xFFE7
#define APP8 0xFFE8
#define APP9 0xFFE9
#define APP10 0xFFEA
#define APP11 0xFFEB
#define APP12 0xFFEC
#define APP13 0xFFED
#define APP14 0xFFEE
#define APP15 0xFFEF
#define DQT  0xFFDB
#define SOF0 0xFFC0
#define SOF1 0xFFC1
#define SOF2 0xFFC2
#define SOF3 0xFFC3
#define DHT  0xFFC4
#define SOS  0xFFDA
#define EOI  0xFFD9
#define COM  0xFFFE

// Define JPEG parser combinators
HParser *jpeg_parser() {
    HParser *marker = h_uint16();
    HParser *length = h_uint16();
    HParser *app_marker = h_choice(h_uint16(), h_uint16(), h_uint16(), h_uint16(), h_uint16(), h_uint16(), h_uint16(), h_uint16(), h_uint16(), h_uint16(), h_uint16(), h_uint16(), h_uint16(), h_uint16(), h_uint16(), h_uint16(), NULL);
    HParser *dqt = h_sequence(marker, length, h_many(h_uint8()), NULL);
    HParser *sof = h_sequence(marker, length, h_uint8(), h_uint16(), h_uint16(), h_uint8(), h_many(h_uint8()), NULL);
    HParser *dht = h_sequence(marker, length, h_many(h_uint8()), NULL);
    HParser *sos = h_sequence(marker, length, h_uint8(), h_many(h_uint8()), NULL);
    HParser *com = h_sequence(marker, length, h_many(h_uint8()), NULL);
    HParser *eoi = h_sequence(marker, NULL);

    HParser *segment = h_choice(dqt, sof, dht, sos, com, eoi, NULL);
    HParser *jpeg = h_sequence(h_uint16(), h_many(segment), NULL);

    return jpeg;
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

    uint8_t *data = (uint8_t *)malloc(file_size);
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
        printf("JPEG parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse JPEG.\n");
    }

    free(data);
    return 0;
}