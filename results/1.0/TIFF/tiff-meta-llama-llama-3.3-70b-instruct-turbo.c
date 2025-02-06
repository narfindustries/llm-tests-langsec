#include <hammer/hammer.h>
#include <stdlib.h>

typedef enum {
    II,
    MM
} ByteOrder;

typedef enum {
    NONE,
    INCH,
    CENTIMETER
} ResolutionUnit;

typedef enum {
    BLACK_IS_ZERO,
    WHITE_IS_ZERO,
    RGB,
    PALETTE_COLOR,
    TRANSPARENCY_MASK,
    CMYK,
    YCBCR
} PhotometricInterpretation;

typedef enum {
    UNCOMPRESSED,
    CCITT_GROUP_3,
    CCITT_GROUP_4,
    LZW,
    PACK_BITS,
    JPEG
} Compression;

typedef enum {
    CHUNKY,
    PLANAR
} PlanarConfiguration;

typedef enum {
    GRAYSCALE,
    RGB,
    CMYK
} ColorSpace;

#define UINT8 1
#define ASCII 2
#define UINT16 3
#define UINT32 4
#define RATIONAL 5
#define SINT8 6
#define UNDEFINED 7
#define SINT16 8
#define SINT32 9
#define SRATIONAL 10
#define FLOAT 11
#define DOUBLE 12

typedef struct {
    uint16_t tag;
    uint16_t type;
    uint32_t count;
    uint32_t valueOffset;
} IFDEntry;

typedef struct {
    uint16_t numberOfTags;
    IFDEntry entries[];
} IFD;

typedef struct {
    uint16_t magic;
    uint32_t offset;
} TIFFHeader;

typedef struct {
    TIFFHeader header;
    IFD ifd;
} TIFF;

HParser* byte_order() {
    return h_choice(
        h_sequence(
            (HParser*)(uintptr_t)"II",
            h_return((void*)(uintptr_t)II)
        ),
        h_sequence(
            (HParser*)(uintptr_t)"MM",
            h_return((void*)(uintptr_t)MM)
        )
    );
}

HParser* tiff_header() {
    return h_sequence(
        byte_order(),
        h_uint16(),
        h_uint32()
    );
}

HParser* ifd_entry() {
    return h_struct(
        h_uint16(),
        h_uint16(),
        h_uint32(),
        h_uint32()
    );
}

HParser* ifd() {
    return h_sequence(
        h_uint16(),
        h_repeat(ifd_entry())
    );
}

HParser* tiff() {
    return h_sequence(
        tiff_header(),
        ifd()
    );
}

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file\n");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t length = ftell(file);
    rewind(file);

    uint8_t* input = malloc(length);
    if (!input) {
        printf("Error allocating memory\n");
        return 1;
    }

    size_t bytesRead = fread(input, 1, length, file);
    if (bytesRead != length) {
        printf("Error reading file\n");
        return 1;
    }

    HParser* parser = tiff();
    HParseResult* result = h_parse(parser, input, length);
    if (!result) {
        printf("Error parsing file\n");
        return 1;
    }

    TIFF* tiff = (TIFF*)result;
    printf("TIFF Magic: %04x\n", tiff->header.magic);
    printf("TIFF Offset: %08x\n", tiff->header.offset);
    printf("Number of Tags: %d\n", tiff->ifd.numberOfTags);

    for (int i = 0; i < tiff->ifd.numberOfTags; i++) {
        IFDEntry* entry = &tiff->ifd.entries[i];
        printf("Tag: %04x\n", entry->tag);
        printf("Type: %04x\n", entry->type);
        printf("Count: %08x\n", entry->count);
        printf("Value Offset: %08x\n", entry->valueOffset);
    }

    return 0;
}