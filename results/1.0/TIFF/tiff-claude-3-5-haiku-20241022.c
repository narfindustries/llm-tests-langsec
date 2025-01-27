#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdint.h>

// Define the TIFF structure and parsing rules
static const HParser *tiff_header;
static const HParser *tiff_ifd;
static const HParser *tiff_entry;
static const HParser *tiff_file;

// TIFF Header Parser
static const HParser* make_tiff_header() {
    return h_sequence(
        h_choice(
            h_literal("II"), // Little Endian
            h_literal("MM"), // Big Endian
            NULL
        ),
        h_uint16(),           // Version number (42)
        h_uint32(),           // First IFD offset
        NULL
    );
}

// TIFF IFD Entry Parser 
static const HParser* make_tiff_entry() {
    return h_sequence(
        h_uint16(),   // Tag
        h_uint16(),   // Type 
        h_uint32(),   // Count
        h_uint32(),   // Value/Offset
        NULL
    );
}

// TIFF IFD Parser
static const HParser* make_tiff_ifd() {
    return h_sequence(
        h_uint16(),   // Number of entries 
        h_repeat_n(h_action(make_tiff_entry(), NULL), 0, 256),
        h_uint32(),   // Next IFD offset
        NULL
    );
}

// Complete TIFF File Parser
static const HParser* make_tiff_file() {
    return h_sequence(
        h_action(make_tiff_header(), NULL),
        h_action(make_tiff_ifd(), NULL),
        NULL
    );
}

// Parser initialization function
void init_parsers() {
    tiff_header = make_tiff_header();
    tiff_entry = make_tiff_entry();
    tiff_ifd = make_tiff_ifd();
    tiff_file = make_tiff_file();
}

// Main parsing function
int parse_tiff(const uint8_t *input, size_t length) {
    init_parsers();
    
    HParseResult *result = h_parse(tiff_file, input, length);
    
    if (result == NULL) {
        fprintf(stderr, "Parsing failed\n");
        return -1;
    }
    
    h_parse_result_free(result);
    return 0;
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <tiff_file>\n", argv[0]);
        return 1;
    }
    
    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }
    
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);
    
    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation failed");
        fclose(file);
        return 1;
    }
    
    size_t read_size = fread(buffer, 1, file_size, file);
    fclose(file);
    
    int result = parse_tiff(buffer, read_size);
    
    free(buffer);
    return result;
}