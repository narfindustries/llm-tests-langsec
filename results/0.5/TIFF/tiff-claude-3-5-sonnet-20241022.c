#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// TIFF Parser Combinators
HParser* tiff_byte_order() {
    return h_choice(h_token((uint8_t*)"II", 2),  // Little-endian
                    h_token((uint8_t*)"MM", 2),  // Big-endian
                    NULL);
}

HParser* tiff_version() {
    return h_int_range(h_uint16(), 42, 42);
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
                     h_repeat_n(tiff_ifd_entry(), h_uint16()),
                     h_uint32(),  // Offset to next IFD
                     NULL);
}

HParser* tiff_header() {
    return h_sequence(tiff_byte_order(),
                     tiff_version(),
                     h_uint32(),  // Offset to first IFD
                     NULL);
}

HParser* tiff_parser() {
    return h_sequence(tiff_header(),
                     h_many(tiff_ifd()),
                     NULL);
}

void print_parse_result(HParsedToken* result) {
    if (!result) {
        printf("Parsing failed\n");
        return;
    }

    // Add result printing logic here based on the parsed structure
    printf("TIFF file parsed successfully\n");
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <tiff_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        fprintf(stderr, "Could not open file: %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        fprintf(stderr, "Memory allocation failed\n");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        fprintf(stderr, "File read failed\n");
        free(buffer);
        fclose(file);
        return 1;
    }

    HParser* parser = tiff_parser();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result) {
        print_parse_result(result->ast);
        h_parse_result_free(result);
    } else {
        printf("Parsing failed\n");
    }

    free(buffer);
    fclose(file);
    return 0;
}