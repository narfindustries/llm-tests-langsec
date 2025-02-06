#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

// Helper functions for easier parsing

// Parses a uint16_t in little-endian format
static HParser uint16_le(void) {
    HParser byte = h_any();
    return h_map2(byte, byte, [](uint8_t a, uint8_t b){ return (uint16_t)a | ((uint16_t)b << 8);});
}

// Parses a uint32_t in little-endian format
static HParser uint32_le(void) {
    HParser byte = h_any();
    return h_map4(byte, byte, byte, byte, [](uint8_t a, uint8_t b, uint8_t c, uint8_t d){
        return (uint32_t)a | ((uint32_t)b << 8) | ((uint32_t)c << 16) | ((uint32_t)d << 24);
    });
}


// Parses a variable-length string
static HParser string(size_t len) {
    HParser byte = h_any();
    return h_count(byte, len);
}

//Helper function to parse variable length fields with length prefix
static HParser varlen_field(void) {
    HParser len = uint16_le();
    HParser byte = h_any();
    return h_bind(len, [byte](size_t l){ return h_count(byte, l);});
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <zip_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char *buffer = (char *)malloc(fsize);
    fread(buffer, 1, fsize, fp);
    fclose(fp);

    HParser localFileHeader = h_sequence(
        uint32_le(), // Signature
        uint16_le(), // Version
        uint16_le(), // Flags
        uint16_le(), // Compression
        uint16_le(), // Modification Time
        uint16_le(), // Modification Date
        uint32_le(), // CRC32
        uint32_le(), // Compressed Size
        uint32_le(), // Uncompressed Size
        uint16_le(), // Filename Length
        uint16_le(), // Extra Field Length
        varlen_field(), // Filename
        varlen_field()  // Extra Field
    );

    HParser centralDirectoryHeader = h_sequence(
        uint32_le(), // Signature
        uint16_le(), // Version Made By
        uint16_le(), // Version Needed to Extract
        uint16_le(), // General Purpose Bit Flag
        uint16_le(), // Compression Method
        uint16_le(), // Last Modified Time
        uint16_le(), // Last Modified Date
        uint32_le(), // CRC-32
        uint32_le(), // Compressed Size
        uint32_le(), // Uncompressed Size
        uint16_le(), // Filename Length
        uint16_le(), // Extra Field Length
        uint16_le(), // File Comment Length
        uint16_le(), // Disk Number Start
        uint16_le(), // Internal File Attributes
        uint32_le(), // External File Attributes
        uint32_le(), // Relative Offset of Local Header
        varlen_field(), //Filename
        varlen_field(), //Extra Field
        varlen_field() //File Comment
    );

    HParser endOfCentralDirectory = h_sequence(
        uint32_le(), // Signature
        uint16_le(), // Number of this disk
        uint16_le(), // Number of the disk with the start of the central directory
        uint16_le(), // Total number of entries in the central directory on this disk
        uint16_le(), // Total number of entries in the central directory
        uint32_le(), // Size of the central directory
        uint32_le(), // Offset of start of central directory
        uint16_le(), // Comment Length
        varlen_field() //Comment
    );


    HParseResult result = h_parse(&localFileHeader, buffer, fsize);

    if (result.success) {
        printf("Parsing successful!\n");
    } else {
        fprintf(stderr, "Parsing failed at offset %zu: %s\n", result.offset, result.error);
    }

    free(buffer);
    return 0;
}
