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
} PngChunk;

uint32_t read_uint32_be(const uint8_t* data) {
    return (data[0] << 24) | (data[1] << 16) | (data[2] << 8) | data[3];
}

uint32_t crc32(const uint8_t* data, size_t len) {
    //REPLACE WITH A PROPER CRC32 IMPLEMENTATION
    uint32_t crc = 0;
    for (size_t i = 0; i < len; i++) {
        crc ^= data[i];
    }
    return crc;
}

HammerParser uint32_be() {
    return map(uint32_parser, read_uint32_be);
}

HammerParser string4() {
    return map(bytes(4), (HammerCombinator) (uint8_t* str) { return (char*)str; });
}

HammerParser uint8_array_parser(HammerParser len_parser) {
    return bind(len_parser, (HammerCombinator) (size_t len) { return bytes(len); });
}

HammerParser png_chunk_parser() {
    return seq4(uint32_be(), string4(), uint8_array_parser(uint32_be()), uint32_be(),
                (HammerCombinator) (uint32_t len, char* type, uint8_t* data, uint32_t crc) {
                    PngChunk* chunk = (PngChunk*)malloc(sizeof(PngChunk));
                    if (chunk == NULL) {
                        perror("Memory allocation failed");
                        exit(1);
                    }
                    chunk->length = len;
                    strncpy(chunk->type, type, 4);
                    chunk->data = data;
                    chunk->crc = crc;
                    return chunk;
                });
}

int main(int argc, char* argv[]) {
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
    long fileSize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t* fileData = (uint8_t*)malloc(fileSize);
    if (fileData == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }
    if (fread(fileData, 1, fileSize, fp) != fileSize) {
        perror("Error reading file");
        fclose(fp);
        free(fileData);
        return 1;
    }
    fclose(fp);

    HammerInput input = {fileData, fileSize};
    HammerResult result = hammer_parse(png_chunk_parser(), &input);

    if (result.success) {
        PngChunk* chunk = (PngChunk*)result.value;
        printf("Chunk Length: %u\n", chunk->length);
        printf("Chunk Type: %s\n", chunk->type);
        free(chunk->data);
        free(chunk);
    } else {
        fprintf(stderr, "Error parsing PNG file: %s\n", result.error);
    }

    free(fileData);
    return 0;
}
