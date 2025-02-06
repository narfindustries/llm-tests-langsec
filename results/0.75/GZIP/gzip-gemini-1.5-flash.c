#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <zlib.h>

// Helper function to read a specific number of bytes from a file.
static size_t read_bytes(FILE *fp, void *buffer, size_t count) {
    return fread(buffer, 1, count, fp);
}

// Parser for a single byte
static hammer_parser_t byte(uint8_t expected) {
    return hammer_satisfy((hammer_predicate_t)(uintptr_t)(int (*)(uint8_t)) (int)(int8_t)expected);
}

// Parser for GZIP header fields
static hammer_parser_t gzip_header() {
    return hammer_seq(
        byte(0x1f),
        byte(0x8b),
        byte(8), // CM: Deflate
        hammer_map(hammer_uint8, [](uint8_t flags){ return flags; }), // FLG
        hammer_uint32, // MTIME
        hammer_uint8, // XFL
        hammer_uint8, // OS
        hammer_maybe(hammer_seq(
            hammer_uint16,
            hammer_bytes(hammer_uint16)
        )), // EXTRA
        hammer_maybe(hammer_until_char(0x00)), // FNAME
        hammer_maybe(hammer_until_char(0x00)), // FCOMMENT
        hammer_maybe(hammer_uint16) // FHCRC
    );
}

// Parser for GZIP trailer fields
static hammer_parser_t gzip_trailer() {
    return hammer_seq(
        hammer_uint32, // CRC32
        hammer_uint32  // ISIZE
    );
}

// Parser for the entire GZIP file
static hammer_parser_t gzip_file() {
    return hammer_seq(
        gzip_header,
        hammer_many(hammer_any), // Compressed Data
        gzip_trailer
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gzip_file>\n", argv[0]);
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

    if (fread(buffer, 1, fileSize, fp) != fileSize) {
        perror("Error reading file");
        free(buffer);
        fclose(fp);
        return 1;
    }
    fclose(fp);

    hammer_result_t result = hammer_parse(gzip_file, buffer, fileSize);

    if (result.success) {
        printf("GZIP file parsed successfully!\n");
        // Access parsed data here.  Note that the structure of result.value
        // will mirror the structure defined in gzip_file parser.
    } else {
        fprintf(stderr, "Error parsing GZIP file: %s at offset %zu\n", result.error_message, result.offset);
    }

    free(buffer);
    return 0;
}

