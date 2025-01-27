#include <hammer/hammer.h>
#include <stdio.h>

static HParser* init_tiff_parser(void) {
    // Basic integer parsers
    HParser* uint16 = h_uint16();
    HParser* uint32 = h_uint32();
    HParser* uint8 = h_uint8();

    // IFD Entry parser
    HParser* ifd_entry = h_sequence(
        h_uint16(), // Tag
        h_uint16(), // Type
        h_uint32(), // Count
        h_uint32(), // Value/Offset
        NULL
    );

    // IFD parser (Directory with entries)
    HParser* ifd = h_sequence(
        h_uint16(), // Number of directory entries
        h_many1(ifd_entry), // Array of IFD entries
        h_uint32(), // Offset to next IFD
        NULL
    );

    // Header parser
    HParser* header = h_sequence(
        h_choice(h_token((const uint8_t*)"II", 2), 
                h_token((const uint8_t*)"MM", 2), NULL), // Byte order
        h_uint16(), // Magic number (42)
        h_uint32(), // Offset to first IFD
        NULL
    );

    // Complete TIFF parser
    return h_sequence(
        header,
        ifd,
        NULL
    );
}

const HParser* tiff_parser = init_tiff_parser();

const HParserBackend* backend = &h_packrat_backend;

bool parse_tiff(const uint8_t* input, size_t length) {
    HParseResult* result = h_parse(tiff_parser, input, length);
    if (result) {
        h_parse_result_free(result);
        return true;
    }
    return false;
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
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

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        fclose(file);
        fprintf(stderr, "Memory allocation failed\n");
        return 1;
    }

    size_t bytes_read = fread(buffer, 1, file_size, file);
    fclose(file);

    if (bytes_read != file_size) {
        free(buffer);
        fprintf(stderr, "Failed to read entire file\n");
        return 1;
    }

    bool success = parse_tiff(buffer, file_size);
    free(buffer);

    if (!success) {
        fprintf(stderr, "Failed to parse TIFF file\n");
        return 1;
    }

    printf("Successfully parsed TIFF file\n");
    return 0;
}