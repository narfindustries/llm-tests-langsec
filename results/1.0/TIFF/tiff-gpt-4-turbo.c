#include <hammer/hammer.h>
#include <hammer/glue.h>

// Basic predefined parsers from Hammer
static HParser *uint8 = h_uint8();
static HParser *uint16le = h_uint16_le();
static HParser *uint16be = h_uint16_be();
static HParser *uint32le = h_uint32_le();
static HParser *uint32be = h_uint32_be();

// TIFF data type definitions
typedef struct {
    uint16_t tag;
    uint16_t type;
    uint32_t length;
    uint32_t value_offset;
} IFDEntry;

typedef struct {
    uint16_t magic;
    uint32_t ifd_offset;
} TIFFHeader;

typedef struct {
    uint32_t entries;
    IFDEntry *ifd_entry;
    uint32_t next_ifd;
} IFD;

// Parser definition for IFDEntry
static HParser *ifd_entry_parser() {
    return h_struct(
        (HMember) { .name = "tag",       .parser = uint16le },
        (HMember) { .name = "type",      .parser = uint16le },
        (HMember) { .name = "length",    .parser = uint32le },
        (HMember) { .name = "value_offset", .parser = uint32le },
        NULL
    );
}

// Parser definition for IFD
static HParser *ifd_parser() {
    return h_sequence(
        h_length_value(h_bind(uint32le, h_length_value(h_bind(uint32le, ifd_entry_parser()))),
            "entries"),
        h_bind(uint32le, ifd_entry_parser()),
        (HParser) {.bit_offset= h_length_value(h_bind(uint32le, ifd_entry_parser()))},
        (HParser) {.bit_offset = .eof = true},
        NULL
    );
}

// Parser for TIFF Header
static HParser *tiff_header_parser() {
    return h_struct(
        (HMember) { .name = "magic",    .parser = uint16be },
        (HMember) { .name = "ifd_offset", .parser = uint32le },
        NULL
    );
}

// Combining into a complete TIFF parser
static HParser *tiff_parser() {
    return h_sequence(
        tiff_header_parser(),
        h_seek(h_indirect(ifd_parser(), "ifd_offset")),
        NULL
    );
}

int main(int argc, char *argv[]) {
    HParseResult *result;
    HParser *tiffP = tiff_parser();

    // Mimicking file reading for demonstration: Normally use h_parse_file
    const uint8_t tiff_data[] = {
        0x4D, 0x4D, 0x00, 0x2A, // Big-endian magic and marker
        0x00, 0x00, 0x00, 0x08, // Offset to first IFD
        // First IFD (at least one entry, then next IFD offset)
        0x01, 0x00, 0x00, 0x01, // 1 entry 
        0x01, 0x02, // Tag, for instance, 0x0102 (ImageWidth)
        0x00, 0x03, // Type: SHORT (3)
        0x00, 0x00, 0x00, 0x01, // Length: 1 
        0x00, 0x00, 0x00, 0x48, // Value offset (where the data for ImageWidth starts)
        0x00, 0x00, 0x00, 0x00  // No more IFDs
    };

    result = h_parse(tiffP, tiff_data, sizeof(tiff_data));
    if (result) {
        printf("TIFF parsing successful.\n");
        h_pprint(stdout, result->ast, 0, 1);  // Pretty print parse tree
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse TIFF data.\n");
    }

    h_parser_free(tiffP);
    return 0;
}