#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct {
    uint16_t major_version;
    uint16_t minor_version;
    uint8_t density_units;
    uint16_t x_density;
    uint16_t y_density;
    uint8_t thumbnail_width;
    uint8_t thumbnail_height;
} JFIFHeader;

typedef struct {
    uint8_t precision;
    uint8_t table_id;
    uint8_t values[64];
} QuantizationTable;

typedef struct {
    uint8_t table_class;
    uint8_t table_destination;
    uint8_t lengths[16];
    uint8_t* values;
} HuffmanTable;

typedef struct {
    uint8_t precision;
    uint16_t height;
    uint16_t width;
    uint8_t num_components;
    uint8_t* component_ids;
    uint8_t* sampling_factors;
    uint8_t* quantization_table_mapping;
} StartOfFrame;

typedef struct {
    uint8_t num_components;
    uint8_t* component_selectors;
    uint8_t* dc_huffman_tables;
    uint8_t* ac_huffman_tables;
    uint8_t spectral_start;
    uint8_t spectral_end;
    uint8_t approximation_high;
    uint8_t approximation_low;
} StartOfScan;

HParseResult* parse_jfif_header(const HParser* parser, const uint8_t* input, size_t length) {
    HParser* major_version = h_uint16();
    HParser* minor_version = h_uint16();
    HParser* density_units = h_uint8();
    HParser* x_density = h_uint16();
    HParser* y_density = h_uint16();
    HParser* thumbnail_width = h_uint8();
    HParser* thumbnail_height = h_uint8();

    HParser* jfif_header = h_sequence(
        major_version, minor_version, density_units,
        x_density, y_density, thumbnail_width, thumbnail_height, NULL
    );

    return h_parse(jfif_header, input, length);
}

HParseResult* parse_quantization_table(const HParser* parser, const uint8_t* input, size_t length) {
    HParser* precision = h_uint8();
    HParser* table_id = h_uint8();
    HParser* values = h_repeat_n(h_uint8(), 64);

    HParser* quantization_table = h_sequence(
        precision, table_id, values, NULL
    );

    return h_parse(quantization_table, input, length);
}

HParseResult* parse_huffman_table(const HParser* parser, const uint8_t* input, size_t length) {
    HParser* table_class = h_uint8();
    HParser* table_destination = h_uint8();
    HParser* lengths = h_repeat_n(h_uint8(), 16);
    HParser* values = h_repeat_n(h_uint8(), 256);

    HParser* huffman_table = h_sequence(
        table_class, table_destination, lengths, values, NULL
    );

    return h_parse(huffman_table, input, length);
}

HParseResult* parse_start_of_frame(const HParser* parser, const uint8_t* input, size_t length) {
    HParser* precision = h_uint8();
    HParser* height = h_uint16();
    HParser* width = h_uint16();
    HParser* num_components = h_uint8();
    HParser* component_ids = h_repeat_n(h_uint8(), 3);
    HParser* sampling_factors = h_repeat_n(h_uint8(), 3);
    HParser* quantization_table_mapping = h_repeat_n(h_uint8(), 3);

    HParser* start_of_frame = h_sequence(
        precision, height, width, num_components,
        component_ids, sampling_factors, quantization_table_mapping, NULL
    );

    return h_parse(start_of_frame, input, length);
}

HParseResult* parse_start_of_scan(const HParser* parser, const uint8_t* input, size_t length) {
    HParser* num_components = h_uint8();
    HParser* component_selectors = h_repeat_n(h_uint8(), 3);
    HParser* dc_huffman_tables = h_repeat_n(h_uint8(), 3);
    HParser* ac_huffman_tables = h_repeat_n(h_uint8(), 3);
    HParser* spectral_start = h_uint8();
    HParser* spectral_end = h_uint8();
    HParser* approximation_high = h_uint8();
    HParser* approximation_low = h_uint8();

    HParser* start_of_scan = h_sequence(
        num_components, component_selectors, dc_huffman_tables,
        ac_huffman_tables, spectral_start, spectral_end,
        approximation_high, approximation_low, NULL
    );

    return h_parse(start_of_scan, input, length);
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <jpeg_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("Error reading file");
        free(buffer);
        fclose(file);
        return 1;
    }
    fclose(file);

    HParser* jpeg_parser = h_sequence(
        h_ch(0xFF), h_ch(0xD8),  // SOI marker
        h_end_p(),
        NULL
    );

    HParseResult* result = h_parse(jpeg_parser, buffer, file_size);
    if (result && result->ast) {
        printf("JPEG parsed successfully\n");
        h_parse_result_free(result);
    } else {
        printf("JPEG parsing failed\n");
    }

    free(buffer);
    return 0;
}