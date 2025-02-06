#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define parsers for basic field types in NITF
HParser *nitf_string(size_t n) {
    return h_bits(n*8, false);
}

HParser *nitf_integer(size_t n) {
    return h_int_range(h_int64(), 0, n);
}

// Define parsers for specific fields
HParser *file_profile_name() {
    return nitf_string(4);
}

HParser *file_version() {
    return nitf_string(5);
}

HParser *complexity_level() {
    return nitf_integer(99);
}

HParser *standard_type() {
    return nitf_string(4);
}

HParser *originating_station_id() {
    return nitf_string(10);
}

HParser *file_title() {
    return nitf_string(80);
}

HParser *file_security_classification() {
    return nitf_string(1);
}

HParser *file_copy_number() {
    return nitf_string(5);
}

HParser *file_number_of_copies() {
    return nitf_string(5);
}

HParser *encryption() {
    return nitf_string(1);
}

HParser *file_background_color() {
    return nitf_string(3);
}

HParser *originator_name() {
    return nitf_string(24);
}

HParser *originator_phone() {
    return nitf_string(18);
}

HParser *file_length() {
    return h_uint64();
}

HParser *file_header_length() {
    return h_uint64();
}

// Combine parsers into a single file header parser
HParser *nitf_file_header() {
    return h_sequence(
        file_profile_name(),
        file_version(),
        complexity_level(),
        standard_type(),
        originating_station_id(),
        file_title(),
        file_security_classification(),
        file_copy_number(),
        file_number_of_copies(),
        encryption(),
        file_background_color(),
        originator_name(),
        originator_phone(),
        file_length(),
        file_header_length(),
        NULL
    );
}

// Main function to parse NITF file
int main(int argc, char **argv) {
    if (argc != 2) {
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

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("Failed to read file");
        free(buffer);
        fclose(file);
        return 1;
    }

    HParser *nitf_parser = nitf_file_header();
    HParseResult *result = h_parse(nitf_parser, buffer, file_size);

    if (result) {
        printf("NITF file parsed successfully.\n");
    } else {
        printf("Failed to parse NITF file.\n");
    }

    free(buffer);
    fclose(file);
    h_parse_result_free(result);
    h_parser_free(nitf_parser);

    return 0;
}