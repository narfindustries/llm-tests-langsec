#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef struct {
    uint16_t byte_order;
    uint16_t version;
    uint32_t ifd_offset;
    
    struct {
        uint16_t tag;
        uint16_t type;
        uint32_t count;
        uint32_t value_or_offset;
    } *ifd_entries;
    size_t ifd_entry_count;

    uint32_t image_width;
    uint32_t image_length;
    uint16_t bits_per_sample;
    uint16_t compression;
    uint16_t photometric_interpretation;
    uint16_t orientation;
    uint16_t samples_per_pixel;
    uint16_t planar_configuration;
    uint16_t resolution_unit;
} TIFFFile;

HParsedToken* parse_tiff_header(void* p) {
    HParser* byte_order = h_choice(h_literal("\x49\x49"), h_literal("\x4D\x4D"), NULL);
    HParser* version = h_literal("\x2A\x00");
    HParser* ifd_offset = h_uint32();

    HParser* header = h_sequence(byte_order, version, ifd_offset, NULL);
    return header;
}

HParsedToken* parse_tiff_ifd(void* p) {
    HParser* tag = h_uint16();
    HParser* type = h_uint16();
    HParser* count = h_uint32();
    HParser* value_or_offset = h_uint32();

    HParser* ifd_entry = h_sequence(tag, type, count, value_or_offset, NULL);
    return ifd_entry;
}

int parse_tiff_file(const char* filename, TIFFFile* tiff_file) {
    FILE* file = fopen(filename, "rb");
    if (!file) return -1;

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser* tiff_parser = h_sequence(
        h_action(parse_tiff_header, NULL, NULL),
        h_action(parse_tiff_ifd, NULL, NULL),
        NULL
    );

    HParseResult* result = h_parse(tiff_parser, buffer, file_size);
    if (result && result->ast) {
        // Populate tiff_file structure from parsed result
        // This would involve extracting values from the parsed AST
    }

    h_parse_result_free(result);
    free(buffer);
    return 0;
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <tiff_file>\n", argv[0]);
        return 1;
    }

    TIFFFile tiff_file = {0};
    if (parse_tiff_file(argv[1], &tiff_file) != 0) {
        fprintf(stderr, "Failed to parse TIFF file\n");
        return 1;
    }

    printf("TIFF File Details:\n");
    printf("Byte Order: 0x%04X\n", tiff_file.byte_order);
    printf("Version: 0x%04X\n", tiff_file.version);

    return 0;
}