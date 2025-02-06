#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

typedef struct {
    uint16_t marker;
    uint16_t length;
    uint8_t* data;
} JPEGMarker;

typedef struct {
    uint8_t major_version;
    uint8_t minor_version;
    uint8_t density_units;
    uint16_t x_density;
    uint16_t y_density;
    uint8_t thumbnail_width;
    uint8_t thumbnail_height;
} JFIF;

typedef struct {
    uint8_t precision;
    uint16_t height;
    uint16_t width;
    uint8_t num_components;
    struct {
        uint8_t id;
        uint8_t sampling_h;
        uint8_t sampling_v;
        uint8_t quant_table;
    } components[255];
} SOFMarker;

typedef struct {
    uint8_t table_class;
    uint8_t table_dest;
    uint8_t code_lengths[16];
    uint8_t* values;
} HuffmanTable;

typedef struct {
    JFIF jfif;
    SOFMarker sof;
    HuffmanTable huffman_tables[4];
    uint8_t* quantization_tables[4];
    uint16_t restart_interval;
} JPEGFile;

static HParsedToken* parse_marker(const HParseResult* p, void* user_data) {
    return h_make_uint(0xFFD8);
}

HParser* jpeg_parser() {
    HParser* soi = h_token("\xFF\xD8", 2);
    HParser* eoi = h_token("\xFF\xD9", 2);
    
    HParser* app0_marker = h_token("\xFF\xE0", 2);
    HParser* sof_marker = h_choice(
        h_token("\xFF\xC0", 2),  // Baseline
        h_token("\xFF\xC2", 2),  // Progressive
        NULL
    );
    
    HParser* dqt_marker = h_token("\xFF\xDB", 2);
    HParser* dht_marker = h_token("\xFF\xC4", 2);
    HParser* dri_marker = h_token("\xFF\xDD", 2);
    HParser* sos_marker = h_token("\xFF\xDA", 2);
    
    HParser* restart_markers = h_choice(
        h_token("\xFF\xD0", 2),
        h_token("\xFF\xD1", 2),
        h_token("\xFF\xD2", 2),
        h_token("\xFF\xD3", 2),
        h_token("\xFF\xD4", 2),
        h_token("\xFF\xD5", 2),
        h_token("\xFF\xD6", 2),
        h_token("\xFF\xD7", 2),
        NULL
    );
    
    HParser* jpeg = h_sequence(
        soi,
        h_many(h_choice(
            app0_marker,
            sof_marker,
            dqt_marker,
            dht_marker,
            dri_marker,
            sos_marker,
            restart_markers,
            NULL
        )),
        eoi,
        NULL
    );
    
    return jpeg;
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
    
    size_t read_size = fread(buffer, 1, file_size, file);
    fclose(file);
    
    if (read_size != file_size) {
        perror("File read error");
        free(buffer);
        return 1;
    }
    
    HParser* parser = jpeg_parser();
    HParseResult* result = h_parse(parser, buffer, file_size);
    
    if (result) {
        printf("Valid JPEG file\n");
        h_parse_result_free(result);
    } else {
        printf("Invalid JPEG file\n");
    }
    
    h_arena_free(parser);
    free(buffer);
    return 0;
}