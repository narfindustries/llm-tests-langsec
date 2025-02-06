#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

// Helper function to read a byte
static hm_parser byte_parser(void) {
    return hm_take(1);
}

// Helper function to read a 2-byte unsigned short
static hm_parser uint16_parser(void) {
    return hm_map(hm_take(2), (hm_map_func) [](const uint8_t *bytes) {
        return (uint16_t)bytes[0] << 8 | bytes[1];
    });
}

// SOI marker parser
static hm_parser soi_parser(void) {
    return hm_seq(byte_parser, byte_parser, (hm_seq_func) [](const uint8_t *bytes) {
        if (bytes[0] == 0xFF && bytes[1] == 0xD8) {
            return (void *)1; // Success
        }
        return NULL; // Failure
    });
}

// APPn marker parser (simplified, no metadata parsing)
static hm_parser appn_parser(void) {
    return hm_seq(byte_parser, byte_parser, uint16_parser, hm_take_n, (hm_seq_func) [](const uint8_t *bytes) {
        uint16_t length = (uint16_t)bytes[2] << 8 | bytes[3];
        if (bytes[0] == 0xFF && bytes[1] >= 0xE0 && bytes[1] <= 0xEF) {
            return (void *)1; //success
        }
        return NULL; //failure
    });
}

// Placeholder for other marker parsers (DQT, DHT, SOF, SOS, EOI) -  These would require significantly more complex parsers to handle the varied data structures within each marker.  This is a substantial undertaking beyond the scope of a simple example.

// Top-level JPEG parser (simplified)
static hm_parser jpeg_parser(void) {
    return hm_many(hm_choice(soi_parser, appn_parser, hm_fail())); // Add other marker parsers here
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <jpeg_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fileSize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buffer = (uint8_t *)malloc(fileSize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }

    fread(buffer, 1, fileSize, fp);
    fclose(fp);

    hm_result result = hm_parse(jpeg_parser(), buffer, fileSize);

    if (result.success) {
        printf("JPEG file parsed successfully!\n");
    } else {
        printf("JPEG parsing failed at offset %zu: %s\n", result.offset, result.error);
    }

    free(buffer);
    return 0;
}

