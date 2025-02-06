#include <hammer/hammer.h>
#include <hammer/harena.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

typedef struct {
    uint16_t marker;
    bool has_length;
    uint16_t length;
    uint8_t* data;
} JPEGMarker;

typedef struct {
    JPEGMarker soi;
    JPEGMarker* app_markers;
    size_t app_marker_count;
    JPEGMarker* dqt_markers;
    size_t dqt_marker_count;
    JPEGMarker* dht_markers;
    size_t dht_marker_count;
    JPEGMarker sof;
    JPEGMarker sos;
    JPEGMarker* restart_markers;
    size_t restart_marker_count;
    JPEGMarker eoi;
} JPEGFile;

static HParser* jpeg_marker;
static HParser* jpeg_soi;
static HParser* jpeg_app_marker;
static HParser* jpeg_dqt_marker;
static HParser* jpeg_dht_marker;
static HParser* jpeg_sof_marker;
static HParser* jpeg_sos_marker;
static HParser* jpeg_restart_marker;
static HParser* jpeg_eoi;

static HParser* create_jpeg_parser() {
    HAllocator* allocator = h_allocator();
    HArena* arena = h_arena_new(allocator);

    uint8_t soi_bytes[] = {0xFF, 0xD8};
    uint8_t eoi_bytes[] = {0xFF, 0xD9};
    
    jpeg_soi = h_token(arena, soi_bytes, 2);
    jpeg_eoi = h_token(arena, eoi_bytes, 2);
    
    uint8_t app_marker_bytes[][2] = {{0xFF, 0xE0}, {0xFF, 0xE1}, {0xFF, 0xEE}};
    jpeg_app_marker = h_choice(arena,
        h_token(arena, app_marker_bytes[0], 2),
        h_token(arena, app_marker_bytes[1], 2),
        h_token(arena, app_marker_bytes[2], 2),
        NULL
    );
    
    uint8_t dqt_marker_bytes[] = {0xFF, 0xDB};
    uint8_t dht_marker_bytes[] = {0xFF, 0xC4};
    
    jpeg_dqt_marker = h_token(arena, dqt_marker_bytes, 2);
    jpeg_dht_marker = h_token(arena, dht_marker_bytes, 2);
    
    uint8_t sof_marker_bytes[][2] = {{0xFF, 0xC0}, {0xFF, 0xC1}, {0xFF, 0xC2}, {0xFF, 0xC3}};
    jpeg_sof_marker = h_choice(arena,
        h_token(arena, sof_marker_bytes[0], 2),
        h_token(arena, sof_marker_bytes[1], 2),
        h_token(arena, sof_marker_bytes[2], 2),
        h_token(arena, sof_marker_bytes[3], 2),
        NULL
    );
    
    uint8_t sos_marker_bytes[] = {0xFF, 0xDA};
    jpeg_sos_marker = h_token(arena, sos_marker_bytes, 2);
    
    uint8_t restart_marker_bytes[][2] = {
        {0xFF, 0xD0}, {0xFF, 0xD1}, {0xFF, 0xD2}, {0xFF, 0xD3},
        {0xFF, 0xD4}, {0xFF, 0xD5}, {0xFF, 0xD6}, {0xFF, 0xD7}
    };
    
    jpeg_restart_marker = h_choice(arena,
        h_token(arena, restart_marker_bytes[0], 2),
        h_token(arena, restart_marker_bytes[1], 2),
        h_token(arena, restart_marker_bytes[2], 2),
        h_token(arena, restart_marker_bytes[3], 2),
        h_token(arena, restart_marker_bytes[4], 2),
        h_token(arena, restart_marker_bytes[5], 2),
        h_token(arena, restart_marker_bytes[6], 2),
        h_token(arena, restart_marker_bytes[7], 2),
        NULL
    );
    
    jpeg_marker = h_choice(arena,
        jpeg_soi,
        jpeg_app_marker,
        jpeg_dqt_marker,
        jpeg_dht_marker,
        jpeg_sof_marker,
        jpeg_sos_marker,
        jpeg_restart_marker,
        jpeg_eoi,
        NULL
    );
    
    return h_many(arena, jpeg_marker);
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
    
    HParser* jpeg_parser = create_jpeg_parser();
    HParseResult* result = h_parse(jpeg_parser, buffer, file_size);
    
    if (result) {
        printf("JPEG file parsed successfully\n");
        h_parse_result_free(result);
    } else {
        printf("JPEG parsing failed\n");
    }
    
    h_arena_free(h_parser_arena(jpeg_parser));
    free(buffer);
    
    return 0;
}