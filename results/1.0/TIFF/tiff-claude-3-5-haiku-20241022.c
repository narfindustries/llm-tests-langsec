#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

typedef struct {
    uint16_t byte_order;
    uint16_t magic_number;
    uint32_t first_ifd_offset;
    
    struct {
        uint16_t tag;
        uint16_t type;
        uint32_t count;
        uint32_t value_or_offset;
    } *ifd_entries;
    size_t ifd_entry_count;
} TIFFImage;

HParser* parse_byte_order(void) {
    return h_choice(
        h_token("\x49\x49", 2),  // Little Endian
        h_token("\x4D\x4D", 2),  // Big Endian
        NULL
    );
}

HParser* parse_magic_number(void) {
    return h_token("\x00\x2A", 2);
}

HParser* parse_ifd_entry(void) {
    return h_sequence(
        h_uint16(),   // Tag
        h_uint16(),   // Type
        h_uint32(),   // Count
        h_uint32(),   // Value or Offset
        NULL
    );
}

TIFFImage* parse_tiff(const uint8_t* data, size_t length) {
    HParser* parser = h_sequence(
        parse_byte_order(),
        parse_magic_number(),
        h_uint32(),  // First IFD offset
        h_many(parse_ifd_entry()),
        NULL
    );
    
    HParseResult* result = h_parse(parser, data, length);
    if (!result) return NULL;
    
    TIFFImage* tiff = malloc(sizeof(TIFFImage));
    // Extract parsed values into TIFFImage structure
    
    h_parse_result_free(result);
    return tiff;
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <tiff_file>\n", argv[0]);
        return 1;
    }
    
    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }
    
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    uint8_t* buffer = malloc(file_size);
    fread(buffer, 1, file_size, file);
    fclose(file);
    
    TIFFImage* tiff = parse_tiff(buffer, file_size);
    if (!tiff) {
        fprintf(stderr, "Failed to parse TIFF file\n");
        free(buffer);
        return 1;
    }
    
    // Print or process TIFF metadata
    
    free(buffer);
    free(tiff);
    return 0;
}