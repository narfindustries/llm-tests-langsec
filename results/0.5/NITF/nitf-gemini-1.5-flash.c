#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define parsers for NITF fields (simplified example)
//  This is a highly simplified example and does not cover the full NITF specification.
//  A complete implementation would be extremely extensive.

// Helper function to parse a fixed-length string
static HParser string_parser(size_t len) {
    return h_string(len);
}

// Helper function to parse a 4-byte integer
static HParser uint32_parser() {
    return h_uint32_be();
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <nitf_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fileSize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char *fileContent = (char *)malloc(fileSize);
    if (fileContent == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }
    fread(fileContent, 1, fileSize, fp);
    fclose(fp);

    HParser fileHeaderParser = h_sequence(
        string_parser(4), // "NITF"
        uint32_parser(),   // Version
        string_parser(2), // Header length
        h_end()
    );

    HParseResult result = h_parse( &fileHeaderParser, fileContent, fileSize);

    if (result.status == H_PARSE_SUCCESS) {
        printf("NITF file parsed successfully!\n");
        // Access parsed data here... (result.value will contain the parsed structure)
    } else {
        fprintf(stderr, "Error parsing NITF file: %s\n", result.error_message);
    }

    free(fileContent);
    return 0;
}
