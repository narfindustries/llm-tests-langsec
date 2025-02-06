#include <hammer/hammer.h>
#include <hammer/allocator.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

typedef struct {
    uint16_t marker;
    uint16_t length;
    uint8_t* data;
} JPEGSegment;

typedef struct {
    uint8_t major_version;
    uint8_t minor_version;
    uint8_t density_units;
    uint16_t x_density;
    uint16_t y_density;
} JFIF;

typedef struct {
    uint8_t precision;
    uint8_t table_id;
    uint16_t* quantization_values;
} QuantizationTable;

typedef struct {
    uint8_t table_class;
    uint8_t table_destination;
    uint8_t* huffman_lengths;
    uint8_t* huffman_values;
} HuffmanTable;

typedef struct {
    uint8_t precision;
    uint16_t height;
    uint16_t width;
    uint8_t num_components;
    uint8_t* component_ids;
    uint8_t* sampling_factors;
    uint8_t* qtable_destinations;
} StartOfFrame;

typedef struct {
    uint16_t restart_interval;
} RestartInterval;

typedef struct {
    JPEGSegment* segments;
    size_t segment_count;
    JFIF* jfif;
    QuantizationTable* qtables;
    HuffmanTable* htables;
    StartOfFrame* sof;
    RestartInterval* dri;
} JPEGFile;

static HParsedToken* parse_soi(const HParseResult* result, void* user_data) {
    HParsedToken* token = h_alloc_token(result->arena);
    token->token_type = TT_UINT;
    token->uint = 0xFFD8;
    return token;
}

static HParsedToken* parse_eoi(const HParseResult* result, void* user_data) {
    HParsedToken* token = h_alloc_token(result->arena);
    token->token_type = TT_UINT;
    token->uint = 0xFFD9;
    return token;
}

static HParsedToken* parse_app0(const HParseResult* result, void* user_data) {
    JPEGFile* jpeg = (JPEGFile*)user_data;
    jpeg->jfif = malloc(sizeof(JFIF));
    
    HCountedArray* seq = result->ast->seq;
    jpeg->jfif->major_version = seq->elements[0]->uint;
    jpeg->jfif->minor_version = seq->elements[1]->uint;
    jpeg->jfif->density_units = seq->elements[2]->uint;
    jpeg->jfif->x_density = seq->elements[3]->uint;
    jpeg->jfif->y_density = seq->elements[4]->uint;
    
    return (HParsedToken*)result->ast;
}

static HParsedToken* parse_dqt(const HParseResult* result, void* user_data) {
    JPEGFile* jpeg = (JPEGFile*)user_data;
    jpeg->qtables = malloc(sizeof(QuantizationTable));
    
    HCountedArray* seq = result->ast->seq;
    jpeg->qtables->precision = seq->elements[0]->uint;
    jpeg->qtables->table_id = seq->elements[1]->uint;
    
    return (HParsedToken*)result->ast;
}

static HParser* jpeg_parser(HArena* arena) {
    uint8_t soi_bytes[] = {0xFF, 0xD8};
    uint8_t eoi_bytes[] = {0xFF, 0xD9};

    HParser* soi = h_token(soi_bytes, 2);
    HParser* eoi = h_token(eoi_bytes, 2);
    
    HParser* app0 = h_action(
        h_sequence(
            h_uint8(),
            h_uint8(),
            h_uint8(),
            h_uint16(),
            h_uint16()
        ), parse_app0, NULL
    );
    
    HParser* dqt = h_action(
        h_sequence(
            h_uint8(),
            h_uint8(),
            h_repeat_n(h_uint16(), 64)
        ), parse_dqt, NULL
    );

    HParser* full_jpeg = h_sequence(soi, app0, dqt, eoi);
    return full_jpeg;
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <jpeg_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Could not open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    fread(buffer, 1, file_size, file);
    fclose(file);

    HArena* arena = h_new_arena();
    HParser* parser = jpeg_parser(arena);
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result && result->ast) {
        printf("JPEG file parsed successfully\n");
    } else {
        printf("JPEG parsing failed\n");
    }

    h_parse_result_free(result);
    h_arena_free(arena);
    free(buffer);

    return 0;
}