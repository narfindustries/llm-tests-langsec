#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define TIFF data types
#define TIFF_BYTE 1
#define TIFF_ASCII 2
#define TIFF_SHORT 3
#define TIFF_LONG 4
#define TIFF_RATIONAL 5
#define TIFF_SBYTE 6
#define TIFF_UNDEFINED 7
#define TIFF_SSHORT 8
#define TIFF_SLONG 9
#define TIFF_SRATIONAL 10
#define TIFF_FLOAT 11
#define TIFF_DOUBLE 12

// Define TIFF tag structure
typedef struct {
    uint16_t tag;
    uint16_t type;
    uint32_t count;
    uint32_t value_offset; // This can be a value or an offset depending on the type and count
} TIFFTag;

// Define TIFF IFD (Image File Directory)
typedef struct {
    uint16_t num_tags;
    TIFFTag *tags;
    uint32_t next_ifd_offset;
} TIFFIFD;

// Hammer parsers for TIFF data types
HParser *hp_tiff_byte;
HParser *hp_tiff_ascii;
HParser *hp_tiff_short;
HParser *hp_tiff_long;
HParser *hp_tiff_rational;
HParser *hp_tiff_sbyte;
HParser *hp_tiff_undefined;
HParser *hp_tiff_sshort;
HParser *hp_tiff_slong;
HParser *hp_tiff_srational;
HParser *hp_tiff_float;
HParser *hp_tiff_double;

// Hammer parser for a single TIFF tag
HParser *hp_tiff_tag;

// Hammer parser for TIFF IFD
HParser *hp_tiff_ifd;

// Initialize Hammer parsers for TIFF data types
void init_tiff_parsers() {
    hp_tiff_byte = h_uint8();
    hp_tiff_ascii = h_ch_range(0, 127);
    hp_tiff_short = h_uint16();
    hp_tiff_long = h_uint32();
    hp_tiff_rational = h_sequence(h_uint32(), h_uint32(), NULL);
    hp_tiff_sbyte = h_int8();
    hp_tiff_undefined = h_uint8();
    hp_tiff_sshort = h_int16();
    hp_tiff_slong = h_int32();
    hp_tiff_srational = h_sequence(h_int32(), h_int32(), NULL);
    hp_tiff_float = h_bits(32, false);
    hp_tiff_double = h_bits(64, false);

    hp_tiff_tag = h_sequence(
        hp_tiff_short, // Tag
        hp_tiff_short, // Type
        hp_tiff_long,  // Count
        hp_tiff_long,  // Value Offset
        NULL
    );

    hp_tiff_ifd = h_sequence(
        hp_tiff_short, // Number of tags
        h_many(hp_tiff_tag), // Tags
        hp_tiff_long, // Next IFD offset
        NULL
    );
}

// Main function to parse TIFF file
int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <TIFF file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    // Initialize TIFF parsers
    init_tiff_parsers();

    // Read the entire file into memory
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }
    fread(buffer, 1, file_size, file);

    // Parse the TIFF file
    HParseResult *result = h_parse(hp_tiff_ifd, buffer, file_size);
    if (result) {
        printf("TIFF file parsed successfully.\n");
    } else {
        fprintf(stderr, "Failed to parse TIFF file.\n");
    }

    // Cleanup
    free(buffer);
    fclose(file);
    return 0;
}