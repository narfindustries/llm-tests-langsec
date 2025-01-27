#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// NITF Parser Combinators
HParser* nitf_file_header() {
    return h_sequence(
        h_token((uint8_t*)"NITF", 4),  // File Type
        h_int_range(h_bits(2, false), 0, 99),  // Version
        h_int_range(h_bits(3, false), 0, 999), // Complexity Level
        h_token((uint8_t*)"LEV", 3),   // Standard Type
        h_bits(10, false),             // Originating Station ID
        h_bits(14, false),             // File Date Time
        h_token((uint8_t*)"999", 3),   // File Title
        h_bits(80, false),             // Security Classification
        h_bits(40, false),             // Copy/Release Instructions
        h_bits(40, false),             // Special Handling Instructions
        h_bits(20, false),             // Encryption
        h_bits(6, false),              // File Length
        h_bits(4, false),              // Header Length
        h_int_range(h_bits(3, false), 0, 999), // Number of Image Segments
        h_int_range(h_bits(3, false), 0, 999), // Number of Graphics
        h_int_range(h_bits(3, false), 0, 999), // Reserved1
        h_int_range(h_bits(3, false), 0, 999), // Reserved2
        h_int_range(h_bits(3, false), 0, 999), // Number of Text Files
        h_int_range(h_bits(3, false), 0, 999)  // Number of Data Extensions
    );
}

HParser* nitf_image_segment() {
    return h_sequence(
        h_token((uint8_t*)"IM", 2),    // Image Segment Identifier
        h_bits(10, false),             // Image ID
        h_bits(14, false),             // Image Date/Time
        h_bits(17, false),             // Target ID
        h_bits(80, false),             // Image Title
        h_bits(1, false),              // Security Classification
        h_bits(40, false),             // Encryption
        h_bits(6, false),              // Image Length
        h_bits(4, false),              // Image Header Length
        h_int_range(h_bits(3, false), 0, 999), // Number of Image Bands
        h_bits(8, false),              // Image Representation
        h_bits(8, false),              // Image Category
        h_bits(2, false),              // Image Compression
        h_bits(1, false),              // Image Mode
        h_bits(4, false),              // Number of Blocks Per Row
        h_bits(4, false),              // Number of Blocks Per Column
        h_bits(4, false),              // Number of Pixels Per Block Horizontal
        h_bits(4, false),              // Number of Pixels Per Block Vertical
        h_bits(3, false),              // Number of Bits Per Pixel
        h_bits(1, false),              // Image Display Level
        h_bits(3, false),              // Image Attachment Level
        h_bits(2, false),              // Image Location
        h_bits(5, false),              // Image Magnification
        h_bits(1, false)               // User Defined Image Data Length
    );
}

HParser* nitf_parser() {
    return h_sequence(
        nitf_file_header(),
        h_many(nitf_image_segment())
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <nitf_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* buffer = malloc(size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, size, file) != size) {
        perror("Failed to read file");
        free(buffer);
        fclose(file);
        return 1;
    }

    HParser* parser = nitf_parser();
    HParseResult* result = h_parse(parser, buffer, size);

    if (result) {
        printf("Successfully parsed NITF file\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse NITF file\n");
    }

    free(buffer);
    fclose(file);
    return 0;
}