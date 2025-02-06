#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef struct {
    uint32_t length;
    char type[4];
    uint8_t* data;
    uint32_t crc;
} Chunk;

HammerParser ChunkParser() {
    return h_seq(
        h_uint32_be(),
        h_string(4),
        h_bytes(h_uint32_be()),
        h_uint32_be(),
        h_map4(&(Chunk){0}, (HammerMapFunc) memcpy)
    );
}

typedef struct {
    uint32_t width;
    uint32_t height;
    uint8_t bit_depth;
    uint8_t color_type;
    uint8_t compression_method;
    uint8_t filter_method;
    uint8_t interlace_method;
} IHDR;

HammerParser IHDRParser() {
    return h_map(h_seq(
                     h_uint32_be(),
                     h_uint32_be(),
                     h_uint8(),
                     h_uint8(),
                     h_uint8(),
                     h_uint8(),
                     h_uint8(),
                     h_uint8()), (HammerMapFunc) memcpy);
}

HammerParser PNGParser() {
    return h_seq(
        h_string("89PNG\r\n\x1a\n"),
        h_map(ChunkParser(), (HammerMapFunc) memcpy),
        h_many(h_map(ChunkParser(), (HammerMapFunc) memcpy)),
        h_map(ChunkParser(), (HammerMapFunc) memcpy),
        &(PNG){0}
    );
}

typedef struct {
    char signature[8];
    Chunk ihdr;
    Chunk* idat;
    size_t idat_count;
    Chunk iend;
} PNG;

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <png_file>\n", argv[0]);
        return 1;
    }

    FILE* fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t* buffer = (uint8_t*)malloc(fsize);
    fread(buffer, 1, fsize, fp);
    fclose(fp);

    HammerParser parser = PNGParser();
    HammerResult result = hammer_parse(parser, buffer, fsize);

    if (result.success) {
        PNG* png = (PNG*)result.value;
        printf("PNG parsed successfully!\n");
        IHDR* ihdr_data = (IHDR*)png->ihdr.data;
        printf("Width: %u\n", ihdr_data->width);
        printf("Height: %u\n", ihdr_data->height);

        free(png->ihdr.data);
        for (size_t i = 0; i < png->idat_count; i++) {
            free(png->idat[i].data);
        }
        free(png->idat);
        free(png->iend.data);
        free(png);
    } else {
        fprintf(stderr, "Parsing failed at offset %zu: %s\n", result.offset, result.error);
        free(result.value);
    }
    free(buffer);
    return 0;
}
