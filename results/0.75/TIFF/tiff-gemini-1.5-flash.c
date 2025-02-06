#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <hammer/hammer.h>

// Define TIFF data types (partial - expand as needed)
typedef struct {
    uint16_t tag;
    uint16_t type;
    uint32_t count;
    uint32_t value; 
} TIFFEntry;

typedef struct {
    uint16_t byteOrder;
    uint16_t version;
    uint32_t offset;
} TIFFHeader;

// Helper function to create a parser for a specific type and field name (for better readability)
HParser create_parser(HParser (*parser_func)(), const char* field_name){
    return parser_func();
}


// Hammer parser combinators for TIFF structures (simplified example)
static HParser tiffHeaderParser() {
    return h_sequence(
        create_parser(h_uint16_parser, "byteOrder"),
        create_parser(h_uint16_parser, "version"),
        create_parser(h_uint32_parser, "offset"),
        h_make_struct_parser(TIFFHeader, byteOrder, version, offset)
    );
}

static HParser tiffEntryParser() {
    return h_sequence(
        create_parser(h_uint16_parser, "tag"),
        create_parser(h_uint16_parser, "type"),
        create_parser(h_uint32_parser, "count"),
        create_parser(h_uint32_parser, "value"),
        h_make_struct_parser(TIFFEntry, tag, type, count, value)
    );
}

// Main function to parse TIFF file
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

    char *buffer = (char *)malloc(fileSize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(file);
        return 1;
    }
    fread(buffer, 1, fileSize, file);
    fclose(file);

    HParser parser = tiffHeaderParser(); 
    HParseResult result = h_parse(parser, buffer, fileSize);

    if (result.status == H_PARSE_OK) {
        TIFFHeader *header = (TIFFHeader *)result.value;
        printf("Byte Order: %u\n", header->byteOrder);
        printf("Version: %u\n", header->version);
        printf("Offset: %u\n", header->offset);
        free(header); //Free the header.
    } else {
        fprintf(stderr, "Parsing failed: %s\n", result.error);
    }

    free(buffer);
    free(result.error); //Free the error message.

    return 0;
}
