#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// TIFF Parser Combinators
HParser* tiff_byte_order() {
    return h_choice(h_token((uint8_t*)"II", 2), h_token((uint8_t*)"MM", 2), NULL);
}

HParser* tiff_version() {
    return h_int_range(h_uint16(), 42, 42);
}

HParser* tiff_ifd_offset() {
    return h_uint32();
}

HParser* tiff_tag() {
    return h_uint16();
}

HParser* tiff_type() {
    return h_int_range(h_uint16(), 1, 12);
}

HParser* tiff_count() {
    return h_uint32();
}

HParser* tiff_value_offset() {
    return h_uint32();
}

HParser* tiff_ifd_entry() {
    return h_sequence(tiff_tag(),
                     tiff_type(),
                     tiff_count(),
                     tiff_value_offset(),
                     NULL);
}

HParser* tiff_ifd() {
    return h_sequence(h_uint16(),  // Number of directory entries
                     h_many1(tiff_ifd_entry()),
                     h_uint32(),   // Offset to next IFD
                     NULL);
}

HParser* tiff_header() {
    return h_sequence(tiff_byte_order(),
                     tiff_version(),
                     tiff_ifd_offset(),
                     NULL);
}

HParser* tiff_parser() {
    return h_sequence(tiff_header(),
                     tiff_ifd(),
                     NULL);
}

void print_parse_result(const HParsedToken* result) {
    if (!result) {
        printf("Parse failed\n");
        return;
    }

    // Add result printing logic here based on the parsed structure
    printf("TIFF file successfully parsed\n");
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

    // Get file size
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    // Read file into buffer
    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    size_t bytes_read = fread(buffer, 1, file_size, file);
    fclose(file);

    if (bytes_read != file_size) {
        fprintf(stderr, "Failed to read entire file\n");
        free(buffer);
        return 1;
    }

    // Parse TIFF
    const HParser* parser = tiff_parser();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result) {
        print_parse_result(result->ast);
        h_parse_result_free(result);
    } else {
        printf("Failed to parse TIFF file\n");
    }

    free(buffer);
    return 0;
}