#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define TIFF_HEADER_SIZE 8

typedef struct {
    uint16_t byteOrder;
    uint16_t version;
    uint32_t ifdOffset;
} TiffHeader;

typedef struct {
    uint16_t tag;
    uint16_t type;
    uint32_t count;
    uint32_t valueOffset;
} TiffTag;

typedef struct {
    uint16_t numTags;
    TiffTag* tags;
    uint32_t nextIFDOffset;
} TiffIFD;

HParser* tiffByteOrder() {
    return h_choice(h_uint16__m(H_ENDIAN_BE), h_uint16__m(H_ENDIAN_LE));
}

HParser* tiffVersion() {
    return h_uint16__m(H_ENDIAN_LE);
}

HParser* tiffIFDOffset() {
    return h_uint32__m(H_ENDIAN_LE);
}

HParser* tiffHeader() {
    return h_sequence(tiffByteOrder(), tiffVersion(), tiffIFDOffset(), h_end_p(), NULL);
}

HParser* tiffTagID() {
    return h_uint16__m(H_ENDIAN_LE);
}

HParser* tiffTagType() {
    return h_uint16__m(H_ENDIAN_LE);
}

HParser* tiffTagCount() {
    return h_uint32__m(H_ENDIAN_LE);
}

HParser* tiffTagValueOffset() {
    return h_uint32__m(H_ENDIAN_LE);
}

HParser* tiffTag() {
    return h_sequence(tiffTagID(), tiffTagType(), tiffTagCount(), tiffTagValueOffset(), h_end_p(), NULL);
}

HParser* tiffIFD() {
    return h_sequence(h_uint16__m(H_ENDIAN_LE), h_many(tiffTag()), h_uint32__m(H_ENDIAN_LE), h_end_p(), NULL);
}

void parseTiffFile(const char* filename) {
    FILE* file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        return;
    }

    fseek(file, 0, SEEK_END);
    long fileSize = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* buffer = (uint8_t*)malloc(fileSize);
    fread(buffer, 1, fileSize, file);
    fclose(file);

    HParseResult* result = h_parse(tiffHeader(), buffer, fileSize);
    if (!result) {
        fprintf(stderr, "Failed to parse TIFF header\n");
        free(buffer);
        return;
    }

    TiffHeader* header = (TiffHeader*)result->ast;
    printf("Byte Order: 0x%04X\n", header->byteOrder);
    printf("Version: 0x%04X\n", header->version);
    printf("IFD Offset: 0x%08X\n", header->ifdOffset);

    result = h_parse(tiffIFD(), buffer + header->ifdOffset, fileSize - header->ifdOffset);
    if (!result) {
        fprintf(stderr, "Failed to parse IFD\n");
        free(buffer);
        return;
    }

    TiffIFD* ifd = (TiffIFD*)result->ast;
    printf("Number of Tags: %d\n", ifd->numTags);
    for (int i = 0; i < ifd->numTags; i++) {
        printf("Tag ID: 0x%04X, Type: 0x%04X, Count: %d, Value Offset: 0x%08X\n",
               ifd->tags[i].tag, ifd->tags[i].type, ifd->tags[i].count, ifd->tags[i].valueOffset);
    }

    free(header);
    free(ifd->tags);
    free(ifd);
    free(buffer);
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input file>\n", argv[0]);
        return 1;
    }

    parseTiffFile(argv[1]);
    return 0;
}