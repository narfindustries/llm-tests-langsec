#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct {
    uint16_t soi;
    uint16_t app0;
    uint16_t length;
    char identifier[5];
    uint16_t version;
    uint8_t units;
    uint16_t x_density;
    uint16_t y_density;
    uint8_t x_thumbnail;
    uint8_t y_thumbnail;
    uint8_t* thumbnail_data;
    uint16_t dqt;
    uint16_t dqt_length;
    uint8_t precision;
    uint8_t* qtable;
    uint16_t sof0;
    uint16_t sof_length;
    uint8_t precision_sof;
    uint16_t height;
    uint16_t width;
    uint8_t num_components;
    uint8_t* component_info;
    uint16_t dht;
    uint16_t dht_length;
    uint8_t* huffman_table;
    uint16_t sos;
    uint16_t sos_length;
    uint8_t num_components_sos;
    uint8_t* sos_component_info;
    uint8_t* spectral_selection;
    uint8_t* successive_approximation;
    uint8_t* encoded_data;
    uint16_t eoi;
} JPEG;

HParser* jpeg_parser() {
    HParser *soi = h_sequence(h_uint8(), h_uint8(), NULL);
    HParser *app0 = h_sequence(h_uint8(), h_uint8(), NULL);
    HParser *length = h_uint16();
    HParser *identifier = h_sequence(h_ch('J'), h_ch('F'), h_ch('I'), h_ch('F'), h_ch('\0'), NULL);
    HParser *version = h_uint16();
    HParser *units = h_uint8();
    HParser *x_density = h_uint16();
    HParser *y_density = h_uint16();
    HParser *x_thumbnail = h_uint8();
    HParser *y_thumbnail = h_uint8();
    HParser *thumbnail_data = h_many(h_uint8());
    HParser *dqt = h_sequence(h_uint8(), h_uint8(), NULL);
    HParser *dqt_length = h_uint16();
    HParser *precision = h_uint8();
    HParser *qtable = h_many(h_uint8());
    HParser *sof0 = h_sequence(h_uint8(), h_uint8(), NULL);
    HParser *sof_length = h_uint16();
    HParser *precision_sof = h_uint8();
    HParser *height = h_uint16();
    HParser *width = h_uint16();
    HParser *num_components = h_uint8();
    HParser *component_info = h_many(h_uint8());
    HParser *dht = h_sequence(h_uint8(), h_uint8(), NULL);
    HParser *dht_length = h_uint16();
    HParser *huffman_table = h_many(h_uint8());
    HParser *sos = h_sequence(h_uint8(), h_uint8(), NULL);
    HParser *sos_length = h_uint16();
    HParser *num_components_sos = h_uint8();
    HParser *sos_component_info = h_many(h_uint8());
    HParser *spectral_selection = h_many(h_uint8());
    HParser *successive_approximation = h_many(h_uint8());
    HParser *encoded_data = h_many(h_uint8());
    HParser *eoi = h_sequence(h_uint8(), h_uint8(), NULL);

    HParser *jpeg = h_sequence(
        soi, app0, length, identifier, version, units, x_density, y_density, x_thumbnail, y_thumbnail, thumbnail_data,
        dqt, dqt_length, precision, qtable,
        sof0, sof_length, precision_sof, height, width, num_components, component_info,
        dht, dht_length, huffman_table,
        sos, sos_length, num_components_sos, sos_component_info, spectral_selection, successive_approximation, encoded_data,
        eoi,
        NULL
    );

    return jpeg;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
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
    fread(data, 1, file_size, file);
    fclose(file);

    HParser *parser = jpeg_parser();
    HParseResult *result = h_parse(parser, data, file_size);
    if (!result) {
        fprintf(stderr, "Parsing failed\n");
        free(data);
        return 1;
    }

    JPEG *jpeg = (JPEG *)result->ast;
    printf("Successfully parsed JPEG file\n");

    h_parse_result_free(result);
    free(data);

    return 0;
}