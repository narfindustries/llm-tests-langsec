#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define the TIFF header structure
typedef struct {
    uint16_t byte_order;
    uint16_t version;
    uint32_t ifd_offset;
} tiff_header_t;

// Define the IFD entry structure
typedef struct {
    uint16_t tag;
    uint16_t type;
    uint32_t count;
    uint32_t value_offset;
} ifd_entry_t;

// Define the IFD structure
typedef struct {
    uint16_t entry_count;
    ifd_entry_t* entries;
    uint32_t next_ifd_offset;
} ifd_t;

// Define the TIFF structure
typedef struct {
    tiff_header_t header;
    ifd_t ifd;
} tiff_t;

// Hammer parser for TIFF header
HParser* tiff_header_parser() {
    return h_sequence(
        h_int16(),  // byte_order
        h_int16(),  // version
        h_int32(),  // ifd_offset
        NULL
    );
}

// Hammer parser for IFD entry
HParser* ifd_entry_parser() {
    return h_sequence(
        h_int16(),  // tag
        h_int16(),  // type
        h_int32(),  // count
        h_int32(),  // value_offset
        NULL
    );
}

// Hammer parser for IFD
HParser* ifd_parser() {
    return h_sequence(
        h_int16(),  // entry_count
        h_many(ifd_entry_parser()),  // entries
        h_int32(),  // next_ifd_offset
        NULL
    );
}

// Hammer parser for TIFF
HParser* tiff_parser() {
    return h_sequence(
        tiff_header_parser(),
        ifd_parser(),
        NULL
    );
}

// Main function to parse TIFF
int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <tiff_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* buffer = (uint8_t*)malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser* parser = tiff_parser();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }

    free(buffer);
    return 0;
}