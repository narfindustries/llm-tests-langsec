#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define PNG chunk types
#define IHDR 0x49484452
#define PLTE 0x504c5445
#define IDAT 0x49444154
#define IEND 0x49454e44
#define tEXt 0x74455874
#define zTXt 0x7a545874
#define bKGD 0x624b4744
#define pHYs 0x70485973
#define gAMA 0x67414d41
#define cHRM 0x6348524d
#define sBIT 0x73424954
#define sRGB 0x73524742
#define iCCP 0x69434350
#define tIME 0x74494d45
#define sPLT 0x73504c54

// Helper function to read a file into memory
unsigned char *read_file(const char *filename, size_t *length) {
    FILE *f = fopen(filename, "rb");
    if (!f) {
        perror("Unable to open file");
        return NULL;
    }

    fseek(f, 0, SEEK_END);
    *length = ftell(f);
    fseek(f, 0, SEEK_SET);

    unsigned char *data = malloc(*length);
    if (!data) {
        perror("Unable to allocate memory");
        fclose(f);
        return NULL;
    }

    if (fread(data, 1, *length, f) != *length) {
        perror("Error reading file");
        free(data);
        fclose(f);
        return NULL;
    }

    fclose(f);
    return data;
}

// Define parsers for PNG chunks
HParser *png_chunk;

void init_parsers() {
    HParser *uint32 = h_uint32();
    HParser *chunk_type = h_uint32();
    HParser *chunk_data = h_length_value(h_uint32(), h_arbitrary_bytes());
    HParser *crc = h_uint32();

    png_chunk = h_sequence(uint32, chunk_type, chunk_data, crc, NULL);
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <png file>\n", argv[0]);
        return 1;
    }

    size_t length;
    unsigned char *data = read_file(argv[1], &length);
    if (!data) {
        return 1;
    }

    init_parsers();

    HParseResult *result = h_parse(png_chunk, data, length);
    if (result) {
        printf("PNG parsed successfully.\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        printf("Failed to parse PNG.\n");
    }

    free(data);
    return 0;
}