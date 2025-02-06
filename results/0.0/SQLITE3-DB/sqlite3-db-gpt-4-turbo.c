#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define constants based on SQLite format
#define SQLITE_HEADER_STRING "SQLite format 3\000"
#define SQLITE_PAGE_SIZE_MIN 512
#define SQLITE_PAGE_SIZE_MAX 32768

// Function prototypes
static void parse_sqlite_file(const char *filename);
static HParser *sqlite_header_parser(void);

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <sqlite_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    parse_sqlite_file(argv[1]);
    return EXIT_SUCCESS;
}

static void parse_sqlite_file(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }

    // Create a parser for SQLite database header
    HParser *header_parser = sqlite_header_parser();

    // Read the header from the file
    uint8_t header[100];
    if (fread(header, sizeof(header), 1, file) != 1) {
        fprintf(stderr, "Failed to read the header\n");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    // Parse the header
    HParseResult *result = h_parse(header_parser, header, sizeof(header));
    if (result == NULL) {
        fprintf(stderr, "Failed to parse the header\n");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    // Successfully parsed the header
    printf("Successfully parsed the SQLite header\n");

    // Clean up
    h_parse_result_free(result);
    fclose(file);
}

static HParser *sqlite_header_parser(void) {
    return h_sequence(
        h_token((const uint8_t *)SQLITE_HEADER_STRING, 16),
        h_uint16(), // Page size
        h_uint8(),  // File format write version
        h_uint8(),  // File format read version
        h_uint8(),  // Reserved space
        h_uint8(),  // Max payload fraction
        h_uint8(),  // Min payload fraction
        h_uint8(),  // Leaf payload fraction
        h_uint32(), // File change counter
        h_uint32(), // Number of pages
        h_uint32(), // First freelist page
        h_uint32(), // Freelist page count
        h_uint32(), // Schema cookie
        h_uint32(), // Schema format number
        h_int32(),  // Default page cache size
        h_uint32(), // Largest B-tree page
        h_uint32(), // Database text encoding
        h_uint32(), // User version
        h_uint32(), // Incremental vacuum mode
        h_uint32(), // Application ID
        h_repeat_n(h_uint8(), 20), // Reserved for expansion
        h_uint32(), // Version-valid-for number
        h_uint32(), // SQLite version number
        NULL
    );
}