#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

// Define basic parsers for TIFF fields
static HParser *p_tiff_header, *p_ifd_entry, *p_ifd, *p_tiff_file;

// Define a simple action to print the tag for demonstration purposes
HParsedToken *print_tag(const HParseResult *p, void *user_data) {
    (void)user_data;  // Unused
    const HParsedToken *token = p->ast;
    uint16_t tag = *(uint16_t *)token->token;
    printf("Tag: %u\n", tag);
    return h_token_u16(tag);
}

// Utility for parsing fields
HParser *parse_field(uint16_t expected_tag, HParser *value_parser) {
    return h_sequence(
        h_action(h_uint16(), print_tag, NULL),
        h_uint16(),  // Type
        value_parser,
        NULL
    );
}

void init_parsers() {
    // TIFF Header Parser (magic number and offset to first IFD)
    p_tiff_header = h_sequence(
        h_choice(
            h_token((const uint8_t *)"\x49\x49\x2A\x00", 4), // Little-endian
            h_token((const uint8_t *)"\x4D\x4D\x00\x2A", 4), // Big-endian
            NULL
        ),
        h_uint32(), // Offset to first IFD
        NULL
    );

    // IFD Entry Parser
    p_ifd_entry = h_sequence(
        h_uint16(), // Tag
        h_uint16(), // Type
        h_uint32(), // Count
        h_uint32(), // Value Offset
        NULL
    );

    // IFD Parser
    p_ifd = h_many(p_ifd_entry);

    // TIFF Full File Parser
    p_tiff_file = h_sequence(
        p_tiff_header,
        h_many(p_ifd),
        NULL
    );
}

void free_parsers() {
    h_delete_parser(p_tiff_header);
    h_delete_parser(p_ifd_entry);
    h_delete_parser(p_ifd);
    h_delete_parser(p_tiff_file);
}

void parse_file(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Unable to open file");
        exit(EXIT_FAILURE);
    }
    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);
    uint8_t *data = malloc(length);
    if (!data) {
        perror("Memory allocation failure");
        fclose(file);
        exit(EXIT_FAILURE);
    }
    fread(data, 1, length, file);
    fclose(file);

    HParseResult *result = h_parse(p_tiff_file, data, length);
    if (result) {
        printf("File parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse the file.\n");
    }
    free(data);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <file.tiff>\n", argv[0]);
        return EXIT_FAILURE;
    }

    init_parsers();
    parse_file(argv[1]);
    free_parsers();

    return EXIT_SUCCESS;
}