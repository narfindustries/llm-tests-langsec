#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct {
    uint16_t tag;
    uint16_t type;
    uint32_t count;
    uint32_t valueOffset;
} TIFF_IFD_Entry;

typedef struct {
    uint16_t numEntries;
    TIFF_IFD_Entry *entries;
    uint32_t nextIFDOffset;
} TIFF_IFD;

HParser *tiffHeaderParser() {
    return h_sequence(
        h_uint16(), // ByteOrder
        h_uint16(), // TIFF identifier
        h_uint32(), // Offset to first IFD
        NULL
    );
}

HParser *tiffIFDParser() {
    return h_sequence(
        h_uint16(), // Number of entries
        h_many1(h_sequence(
            h_uint16(), // Tag
            h_uint16(), // Type
            h_uint32(), // Count
            h_uint32(), // Value/Offset
            NULL
        )),
        h_uint32(), // Offset to next IFD
        NULL
    );
}

void parseTIFF(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(1);
    }
    fseek(file, 0, SEEK_END);
    long fileSize = ftell(file);
    fseek(file, 0, SEEK_SET);
    uint8_t *buffer = malloc(fileSize);
    fread(buffer, 1, fileSize, file);
    fclose(file);

    HParseResult *result = h_parse(tiffHeaderParser(), buffer, fileSize);
    if (!result) {
        printf("Failed to parse TIFF header\n");
        free(buffer);
        exit(1);
    }

    const HParsedToken *header = result->ast;
    uint32_t ifdOffset = *(uint32_t *)header->bitstring->data;
    h_parse_result_free(result);

    while (ifdOffset != 0) {
        result = h_parse(tiffIFDParser(), buffer + ifdOffset, fileSize - ifdOffset);
        if (!result) {
            printf("Failed to parse IFD at offset %u\n", ifdOffset);
            free(buffer);
            exit(1);
        }

        const HParsedToken *ifdResult = result->ast;
        uint16_t numEntries = *(uint16_t *)ifdResult->bitstring->data;
        printf("IFD with %d entries\n", numEntries);

        const HParsedToken *entries = ifdResult->seq->elements[1];
        for (int i = 0; i < numEntries; i++) {
            const HParsedToken *entry = entries->seq->elements[i];
            uint16_t tag = *(uint16_t *)entry->seq->elements[0]->bitstring->data;
            uint16_t type = *(uint16_t *)entry->seq->elements[1]->bitstring->data;
            uint32_t count = *(uint32_t *)entry->seq->elements[2]->bitstring->data;
            uint32_t valueOffset = *(uint32_t *)entry->seq->elements[3]->bitstring->data;
            printf("Tag: %d, Type: %d, Count: %d, Value/Offset: %u\n", tag, type, count, valueOffset);
        }

        ifdOffset = *(uint32_t *)ifdResult->seq->elements[2]->bitstring->data;
        h_parse_result_free(result);
    }

    free(buffer);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s <TIFF file>\n", argv[0]);
        return 1;
    }
    parseTIFF(argv[1]);
    return 0;
}