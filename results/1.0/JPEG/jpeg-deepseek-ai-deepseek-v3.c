#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

HParser *jpeg_parser;

void init_jpeg_parser() {
    HParser *SOI = h_sequence(h_byte(0xFF), h_byte(0xD8), NULL);
    HParser *EOI = h_sequence(h_byte(0xFF), h_byte(0xD9), NULL);
    HParser *SOF0 = h_sequence(h_byte(0xFF), h_byte(0xC0), NULL);
    HParser *SOF1 = h_sequence(h_byte(0xFF), h_byte(0xC1), NULL);
    HParser *SOF2 = h_sequence(h_byte(0xFF), h_byte(0xC2), NULL);
    HParser *SOF3 = h_sequence(h_byte(0xFF), h_byte(0xC3), NULL);
    HParser *DHT = h_sequence(h_byte(0xFF), h_byte(0xC4), NULL);
    HParser *DQT = h_sequence(h_byte(0xFF), h_byte(0xDB), NULL);
    HParser *DRI = h_sequence(h_byte(0xFF), h_byte(0xDD), NULL);
    HParser *SOS = h_sequence(h_byte(0xFF), h_byte(0xDA), NULL);
    HParser *RSTn = h_sequence(h_byte(0xFF), h_choice(h_byte(0xD0), h_byte(0xD1), h_byte(0xD2), h_byte(0xD3), h_byte(0xD4), h_byte(0xD5), h_byte(0xD6), h_byte(0xD7), NULL), NULL);
    HParser *APPn = h_sequence(h_byte(0xFF), h_choice(h_byte(0xE0), h_byte(0xE1), h_byte(0xE2), h_byte(0xE3), h_byte(0xE4), h_byte(0xE5), h_byte(0xE6), h_byte(0xE7), h_byte(0xE8), h_byte(0xE9), h_byte(0xEA), h_byte(0xEB), h_byte(0xEC), h_byte(0xED), h_byte(0xEE), h_byte(0xEF), NULL), NULL);
    HParser *COM = h_sequence(h_byte(0xFF), h_byte(0xFE), NULL);

    HParser *frame_header = h_sequence(SOF0, h_uint8(), h_uint16(), h_uint16(), h_uint8(), NULL);
    HParser *scan_header = h_sequence(SOS, h_uint8(), NULL);
    HParser *quantization_table = h_sequence(DQT, h_uint8(), NULL);
    HParser *huffman_table = h_sequence(DHT, h_uint8(), h_uint8(), NULL);
    HParser *restart_interval = h_sequence(DRI, h_uint16(), NULL);
    HParser *application_specific = h_sequence(APPn, h_uint16(), NULL);
    HParser *comment = h_sequence(COM, h_uint16(), NULL);

    jpeg_parser = h_sequence(SOI, h_many(h_choice(frame_header, scan_header, quantization_table, huffman_table, restart_interval, application_specific, comment, RSTn, NULL)), EOI, NULL);
}

void parse_jpeg(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        return;
    }
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    uint8_t *data = (uint8_t *)malloc(file_size);
    fread(data, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(jpeg_parser, data, file_size);
    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }
    free(data);
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <JPEG file>\n", argv[0]);
        return 1;
    }
    init_jpeg_parser();
    parse_jpeg(argv[1]);
    return 0;
}