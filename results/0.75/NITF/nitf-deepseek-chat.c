#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define the NITF header structure
typedef struct {
    uint8_t fileProfileName[4];
    uint8_t fileVersion[2];
    uint8_t complexLevel[2];
    uint8_t standardType[4];
    uint8_t originatingStationID[10];
    uint8_t fileDateTime[14];
    uint8_t fileTitle[80];
    uint8_t classification[1];
    uint8_t encryption[1];
    uint8_t fileCopyNumber[5];
    uint8_t fileNumberOfCopies[5];
    uint8_t fileLength[12];
    uint8_t headerLength[6];
} NITFHeader;

// Define the Hammer parser for the NITF header
HParser *nitf_header_parser() {
    return h_sequence(
        h_bytes(4),  // fileProfileName
        h_bytes(2),  // fileVersion
        h_bytes(2),  // complexLevel
        h_bytes(4),  // standardType
        h_bytes(10), // originatingStationID
        h_bytes(14), // fileDateTime
        h_bytes(80), // fileTitle
        h_bytes(1),  // classification
        h_bytes(1),  // encryption
        h_bytes(5),  // fileCopyNumber
        h_bytes(5),  // fileNumberOfCopies
        h_bytes(12), // fileLength
        h_bytes(6),  // headerLength
        NULL
    );
}

// Main function to parse the NITF file
int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <nitf_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = (uint8_t *)malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = nitf_header_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("NITF header parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse NITF header.\n");
    }

    free(buffer);
    return 0;
}