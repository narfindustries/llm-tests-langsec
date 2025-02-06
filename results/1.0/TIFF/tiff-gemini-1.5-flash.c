#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

// Define TIFF data types (incomplete - needs expansion for all types)
typedef struct {
    uint16_t tag;
    uint16_t type;
    uint32_t count;
    uint8_t *value;
} TIFFField;

typedef struct {
    uint16_t byteOrder; // 0x4949 (little-endian) or 0x4D4D (big-endian)
    uint16_t version;   // 42
    uint32_t offset;    // Offset to IFD
} TIFFHeader;

typedef struct {
    TIFFHeader header;
    // ... (rest of TIFF structure - IFD, IFD entries, etc.)
} TIFFImage;


// Parser combinators
HParser byteOrderParser() {
    return h_choice(h_string("II"), h_string("MM"));
}

HParser versionParser() {
    return h_uint16();
}

HParser offsetParser() {
    return h_uint32();
}

HParser headerParser() {
    return h_sequence(
        byteOrderParser(),
        versionParser(),
        offsetParser(),
        h_end()
    );
}


// Main function
int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <tiff_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long fileSize = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = (uint8_t *)malloc(fileSize);
    if (buffer == NULL) {
        perror("Error allocating memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, fileSize, file);
    fclose(file);

    // Parsing using Hammer
    HParser p = headerParser();
    HParseResult r = h_parse(&p, buffer, fileSize);

    if (r.status == H_PARSE_SUCCESS) {
        TIFFHeader *header = (TIFFHeader *)r.value;
        printf("Byte Order: 0x%04X\n", header->byteOrder);
        printf("Version: %u\n", header->version);
        printf("Offset: %u\n", header->offset);
        free(header);
        free(r.value);
    } else {
        fprintf(stderr, "Parsing failed at offset %zu: %s\n", r.offset, r.error);
        free(r.value);
    }

    free(buffer);
    return 0;
}
