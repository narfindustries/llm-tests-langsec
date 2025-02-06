#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Action functions to process parsing results into usable formats
static HParsedToken* act_uint16le(const HParseResult* p) {
    uint8_t* bytes = h_seq_bytes(p->ast);
    uint16_t result = (uint16_t)bytes[0] | ((uint16_t)bytes[1] << 8);
    return H_MAKE_UINT(result);
}

static HParsedToken* act_uint32le(const HParseResult* p) {
    uint8_t* bytes = h_seq_bytes(p->ast);
    uint32_t result = (uint32_t)bytes[0] | ((uint32_t)bytes[1] << 8) | ((uint32_t)bytes[2] << 16) | ((uint32_t)bytes[3] << 24);
    return H_MAKE_UINT(result);
}

// Parser for the fields in the ZIP file header
HParser* zip_signature() {
    return h_bits(32, false);
}

HParser* zip_version() {
    return h_action(h_bits(16, false), act_uint16le, NULL);
}

HParser* zip_general_purpose() {
    return h_action(h_bits(16, false), act_uint16le, NULL);
}

HParser* zip_compression() {
    return h_action(h_bits(16, false), act_uint16le, NULL);
}

HParser* zip_mod_time() {
    return h_action(h_bits(16, false), act_uint16le, NULL);
}

HParser* zip_mod_date() {
    return h_action(h_bits(16, false), act_uint16le, NULL);
}

HParser* zip_crc32() {
    return h_action(h_bits(32, false), act_uint32le, NULL);
}

HParser* zip_compressed_size() {
    return h_action(h_bits(32, false), act_uint32le, NULL);
}

HParser* zip_uncompressed_size() {
    return h_action(h_bits(32, false), act_uint32le, NULL);
}

HParser* zip_file_name_length() {
    return h_action(h_bits(16, false), act_uint16le, NULL);
}

HParser* zip_extra_field_length() {
    return h_action(h_bits(16, false), act_uint16le, NULL);
}

HParser* zip_file_name(HParser* length_parser) {
    return h_length_value(length_parser, h_uint8());
}

HParser* zip_extra_field(HParser* length_parser) {
    return h_length_value(length_parser, h_uint8());
}

HParser* zip_local_file_header() {
    HParser* file_name_length = zip_file_name_length();
    HParser* extra_field_length = zip_extra_field_length();
    return h_sequence(
        zip_signature(),
        zip_version(),
        zip_general_purpose(),
        zip_compression(),
        zip_mod_time(),
        zip_mod_date(),
        zip_crc32(),
        zip_compressed_size(),
        zip_uncompressed_size(),
        file_name_length,
        extra_field_length,
        zip_file_name(file_name_length),
        zip_extra_field(extra_field_length),
        NULL
    );
}

// Main function to parse a ZIP file
void parse_zip_file(const char* filename) {
    FILE* f = fopen(filename, "rb");
    if (!f) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t* buffer = malloc(size);
    fread(buffer, size, 1, f);
    fclose(f);

    HParser* parser = zip_local_file_header();
    HParseResult* result = h_parse(parser, buffer, size);
    if (result) {
        printf("Parsing successful.\n");
        h_pprint(stdout, result->ast, 0, 4);
        h_parse_result_free(result);
    } else {
        printf("Parsing failed.\n");
    }

    h_parser_free(parser);
    free(buffer);
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <zip_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    parse_zip_file(argv[1]);
    return EXIT_SUCCESS;
}