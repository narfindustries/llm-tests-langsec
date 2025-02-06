#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>
#include <hammer/hammer_ext.h>

typedef enum {
    LITTLE_ENDIAN_VALUE = 0x4949,
    BIG_ENDIAN_VALUE = 0x4D4D
} ByteOrderEnum;

typedef struct {
    uint16_t tag;
    uint16_t type;
    uint32_t count;
    uint32_t value_or_offset;
} IFDEntry;

typedef struct {
    ByteOrderEnum byte_order;
    uint16_t magic_number;
    uint32_t ifd_offset;
    uint16_t num_entries;
    IFDEntry* entries;
} TIFFHeader;

static HParsedToken* parse_byte_order(const HParseResult* result, void* user_data) {
    return h_make_uint(result->ast->token_type == TT_SEQUENCE ? LITTLE_ENDIAN_VALUE : BIG_ENDIAN_VALUE);
}

static HParsedToken* parse_magic_number(const HParseResult* result, void* user_data) {
    return h_make_uint(0x2A);
}

static HParsedToken* parse_ifd_entry(const HParseResult* result, void* user_data) {
    HCountedArray* arr = result->ast->seq;
    IFDEntry* entry = malloc(sizeof(IFDEntry));
    
    entry->tag = h_cast_uint(arr->elements[0]);
    entry->type = h_cast_uint(arr->elements[1]);
    entry->count = h_cast_uint(arr->elements[2]);
    entry->value_or_offset = h_cast_uint(arr->elements[3]);

    return h_make_ptr(entry);
}

static HParsedToken* parse_tiff_header(const HParseResult* result, void* user_data) {
    HCountedArray* arr = result->ast->seq;
    TIFFHeader* header = malloc(sizeof(TIFFHeader));
    
    header->byte_order = h_cast_uint(arr->elements[0]);
    header->magic_number = h_cast_uint(arr->elements[1]);
    header->ifd_offset = h_cast_uint(arr->elements[2]);
    header->num_entries = h_cast_uint(arr->elements[3]);
    header->entries = h_cast_ptr(arr->elements[4]);

    return h_make_ptr(header);
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
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("File read error");
        free(buffer);
        fclose(file);
        return 1;
    }
    fclose(file);

    HParser* tag = h_uint16();
    HParser* type = h_uint16();
    HParser* count = h_uint32();
    HParser* value_or_offset = h_uint32();

    HParser* byte_order = h_action(
        h_choice(h_literal("\x49\x49"), h_literal("\x4D\x4D"), NULL),
        parse_byte_order,
        NULL
    );

    HParser* magic_number = h_action(
        h_literal("\x2A\x00"),
        parse_magic_number,
        NULL
    );

    HParser* ifd_entry_parser = h_action(
        h_sequence(tag, type, count, value_or_offset, NULL),
        parse_ifd_entry,
        NULL
    );

    HParser* tiff_parser = h_action(
        h_sequence(
            byte_order,
            magic_number,
            h_uint32(),
            h_uint16(),
            h_many1(ifd_entry_parser),
            NULL
        ),
        parse_tiff_header,
        NULL
    );

    HParseResult* result = h_parse(tiff_parser, buffer, file_size);
    if (!result || !result->ast) {
        fprintf(stderr, "TIFF parsing failed\n");
        free(buffer);
        return 1;
    }

    TIFFHeader* header = h_cast_ptr(result->ast);
    printf("Byte Order: %s\n", header->byte_order == LITTLE_ENDIAN_VALUE ? "Little Endian" : "Big Endian");
    printf("Magic Number: 0x%04X\n", header->magic_number);
    printf("IFD Offset: %u\n", header->ifd_offset);
    printf("Number of Entries: %u\n", header->num_entries);

    free(buffer);
    return 0;
}