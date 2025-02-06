#include <hammer/hammer.h>
#include <hammer/harena.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

// JPEG Marker Definitions
#define JPEG_SOI 0xFFD8
#define JPEG_EOI 0xFFD9
#define JPEG_APP0 0xFFE0
#define JPEG_APP1 0xFFE1
#define JPEG_DQT 0xFFDB
#define JPEG_DHT 0xFFC4
#define JPEG_SOF0 0xFFC0
#define JPEG_SOF1 0xFFC1
#define JPEG_SOF2 0xFFC2
#define JPEG_SOF3 0xFFC3
#define JPEG_SOS 0xFFDA
#define JPEG_RST0 0xFFD0
#define JPEG_RST7 0xFFD7

typedef struct {
    uint16_t marker;
    uint16_t length;
    uint8_t* data;
} JpegSegment;

typedef struct {
    JpegSegment soi;
    JpegSegment* app_markers;
    JpegSegment* quantization_tables;
    JpegSegment* huffman_tables;
    JpegSegment frame_header;
    JpegSegment scan_header;
    JpegSegment* restart_markers;
    JpegSegment eoi;
} JpegFile;

HParser* parse_marker() {
    return h_uint16();
}

HParser* parse_segment_length() {
    return h_uint16();
}

HParser* jpeg_parser() {
    HParser* soi_parser = h_token((const uint8_t[]){0xFF, 0xD8}, 2);
    HParser* eoi_parser = h_token((const uint8_t[]){0xFF, 0xD9}, 2);
    
    HParser* app_marker_parser = h_choice(
        h_token((const uint8_t[]){0xFF, 0xE0}, 2),
        h_token((const uint8_t[]){0xFF, 0xE1}, 2),
        NULL
    );
    
    HParser* dqt_parser = h_token((const uint8_t[]){0xFF, 0xDB}, 2);
    HParser* dht_parser = h_token((const uint8_t[]){0xFF, 0xC4}, 2);
    
    HParser* sof_parser = h_choice(
        h_token((const uint8_t[]){0xFF, 0xC0}, 2),
        h_token((const uint8_t[]){0xFF, 0xC1}, 2),
        h_token((const uint8_t[]){0xFF, 0xC2}, 2),
        h_token((const uint8_t[]){0xFF, 0xC3}, 2),
        NULL
    );
    
    HParser* sos_parser = h_token((const uint8_t[]){0xFF, 0xDA}, 2);
    
    HParser* restart_parser = h_choice(
        h_token((const uint8_t[]){0xFF, 0xD0}, 2),
        h_token((const uint8_t[]){0xFF, 0xD7}, 2),
        NULL
    );

    return h_sequence(
        soi_parser,
        h_many(app_marker_parser),
        h_many(dqt_parser),
        h_many(dht_parser),
        sof_parser,
        sos_parser,
        h_optional(h_many(restart_parser)),
        eoi_parser,
        NULL
    );
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

    HArena* arena = h_arena_new();
    HParser* parser = jpeg_parser();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result && result->ast) {
        printf("Valid JPEG file parsed successfully\n");
        h_parse_result_free(result);
    } else {
        printf("Invalid JPEG file\n");
    }

    h_arena_free(arena);
    free(buffer);
    return 0;
}