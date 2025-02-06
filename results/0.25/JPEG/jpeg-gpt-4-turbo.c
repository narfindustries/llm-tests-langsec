#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// JPEG Markers
#define SOI 0xFFD8
#define EOI 0xFFD9
#define SOS 0xFFDA
#define DQT 0xFFDB
#define DHT 0xFFC4
#define SOF0 0xFFC0
#define SOF2 0xFFC2
#define APP0 0xFFE0
#define APP1 0xFFE1
#define COM 0xFFFE

// Function Prototypes
static void parse_jpeg(const char *filename);
static HParser *jpeg_parser();

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <JPEG file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    parse_jpeg(argv[1]);
    return EXIT_SUCCESS;
}

static void parse_jpeg(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }

    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    if (fread(data, 1, size, file) != size) {
        perror("Failed to read file");
        free(data);
        fclose(file);
        exit(EXIT_FAILURE);
    }

    fclose(file);

    HParser *parser = jpeg_parser();
    HParseResult *result = h_parse(parser, data, size);
    if (result) {
        printf("JPEG parsed successfully.\n");
    } else {
        fprintf(stderr, "Failed to parse JPEG.\n");
    }

    h_parse_result_free(result);
    h_parser_unref(parser);
    free(data);
}

static HParser *jpeg_parser() {
    HParser *marker = h_uint16();
    HParser *length = h_uint16();
    HParser *data = h_uint8();

    HParser *segment = h_sequence(marker, length, h_repeat_n(data, h_length_value(length, h_int_sub(h_int64(2), h_int64(0)))), NULL);

    HParser *soi = h_token_u16(SOI);
    HParser *eoi = h_token_u16(EOI);
    HParser *sos = h_sequence(h_token_u16(SOS), length, h_repeat_n(data, h_length_value(length, h_int_sub(h_int64(2), h_int64(0)))), NULL);
    HParser *dqt = h_sequence(h_token_u16(DQT), length, h_repeat_n(data, h_length_value(length, h_int_sub(h_int64(2), h_int64(0)))), NULL);
    HParser *dht = h_sequence(h_token_u16(DHT), length, h_repeat_n(data, h_length_value(length, h_int_sub(h_int64(2), h_int64(0)))), NULL);
    HParser *sof0 = h_sequence(h_token_u16(SOF0), length, h_repeat_n(data, h_length_value(length, h_int_sub(h_int64(2), h_int64(0)))), NULL);
    HParser *sof2 = h_sequence(h_token_u16(SOF2), length, h_repeat_n(data, h_length_value(length, h_int_sub(h_int64(2), h_int64(0)))), NULL);
    HParser *app0 = h_sequence(h_token_u16(APP0), length, h_repeat_n(data, h_length_value(length, h_int_sub(h_int64(2), h_int64(0)))), NULL);
    HParser *app1 = h_sequence(h_token_u16(APP1), length, h_repeat_n(data, h_length_value(length, h_int_sub(h_int64(2), h_int64(0)))), NULL);
    HParser *com = h_sequence(h_token_u16(COM), length, h_repeat_n(data, h_length_value(length, h_int_sub(h_int64(2), h_int64(0)))), NULL);

    HParser *jpeg = h_sequence(soi, h_many(segment), sos, h_many(segment), eoi, NULL);

    return jpeg;
}