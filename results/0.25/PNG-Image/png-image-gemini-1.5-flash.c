#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef struct {
    uint32_t length;
    char type[5];
    uint8_t* data;
    uint32_t crc;
} PngChunk;

uint32_t read_uint32_be(const uint8_t* data) {
    return (data[0] << 24) | (data[1] << 16) | (data[2] << 8) | data[3];
}

uint32_t calculate_crc32(const uint8_t* data, size_t len) {
    //Implementation omitted for brevity. Use a library function.
    return 0; // Placeholder
}

HParser<PngChunk> parse_png_chunk() {
    return sequence(
        uint32_be(),
        string(4),
        bytes(uint32_be()),
        uint32_be(),
        [](uint32_t len, const char* type, uint8_t* data, uint32_t crc){
            PngChunk chunk;
            chunk.length = len;
            strncpy(chunk.type, type, 4);
            chunk.type[4] = '\0';
            chunk.data = data;
            chunk.crc = crc;
            return chunk;
        }
    );
}

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
    long fileSize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t* fileData = (uint8_t*)malloc(fileSize);
    fread(fileData, 1, fileSize, fp);
    fclose(fp);

    HParser<PngChunk*> pngParser = many(parse_png_chunk()); // Changed to PngChunk*

    HResult<PngChunk**> result = pngParser.parse(fileData, fileSize); // Changed to PngChunk**

    if (result.is_success()) {
        printf("PNG file parsed successfully.\n");
        PngChunk** chunks = result.value();
        for (int i = 0; chunks[i] != NULL; i++) {
            printf("Chunk Type: %s, Length: %u, CRC: %u\n", chunks[i]->type, chunks[i]->length, chunks[i]->crc);
            free(chunks[i]->data);
            free(chunks[i]);
        }
        free(chunks);
    } else {
        fprintf(stderr, "Error parsing PNG file: %s\n", result.error().c_str());
    }

    free(fileData);
    return 0;
}
