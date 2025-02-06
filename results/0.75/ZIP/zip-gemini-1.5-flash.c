#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef enum {
    HSuccess,
    HError
} HResult;

static HResult read_uint16(HParser* parser, uint16_t* value) {
    return h_parse_uint16(parser, value);
}

static HResult read_uint32(HParser* parser, uint32_t* value) {
    return h_parse_uint32(parser, value);
}

static HResult read_string(HParser* parser, char** str, uint16_t len) {
    char* buffer = (char*)malloc(len + 1);
    if (buffer == NULL) return HError;
    HResult result = h_parse_bytes(parser, buffer, len);
    if (h_is_err(result)) {
        free(buffer);
        return result;
    }
    buffer[len] = '\0';
    *str = buffer;
    return HSuccess;
}

static HParser* parse_local_file_header() {
    return h_sequence(
        h_parse_uint32(NULL, NULL),
        h_map(read_uint16, NULL),
        h_map(read_uint16, NULL),
        h_map(read_uint16, NULL),
        h_map(read_uint16, NULL),
        h_map(read_uint16, NULL),
        h_map(read_uint32, NULL),
        h_map(read_uint32, NULL),
        h_map(read_uint32, NULL),
        h_map(read_uint16, NULL),
        h_map(read_uint16, NULL),
        h_map2(read_string, NULL),
        h_map2(read_string, NULL)
    );
}

static HParser* parse_central_directory_entry() {
    return h_sequence(
        h_parse_uint32(NULL, NULL),
        h_map(read_uint16, NULL),
        h_map(read_uint16, NULL),
        h_map(read_uint16, NULL),
        h_map(read_uint16, NULL),
        h_map(read_uint16, NULL),
        h_map(read_uint16, NULL),
        h_map(read_uint32, NULL),
        h_map(read_uint32, NULL),
        h_map(read_uint32, NULL),
        h_map(read_uint16, NULL),
        h_map(read_uint16, NULL),
        h_map(read_uint16, NULL),
        h_map(read_uint16, NULL),
        h_map(read_uint16, NULL),
        h_map(read_uint32, NULL),
        h_map(read_uint32, NULL),
        h_map2(read_string, NULL),
        h_map2(read_string, NULL),
        h_map2(read_string, NULL)
    );
}

static HParser* parse_end_of_central_directory() {
    return h_sequence(
        h_parse_uint32(NULL, NULL),
        h_map(read_uint16, NULL),
        h_map(read_uint16, NULL),
        h_map(read_uint16, NULL),
        h_map(read_uint16, NULL),
        h_map(read_uint32, NULL),
        h_map(read_uint32, NULL),
        h_map(read_uint16, NULL),
        h_map2(read_string, NULL)
    );
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <zip_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long fileSize = ftell(file);
    fseek(file, 0, SEEK_SET);

    char* buffer = (char*)malloc(fileSize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, fileSize, file);
    fclose(file);

    HParser* parser = h_new_parser(buffer, fileSize);
    HParseResult* result = h_parse(parser, parse_local_file_header());

    if (h_is_err(result->result)) {
        fprintf(stderr, "Parsing failed\n");
    } else {
        printf("Parsing succeeded!\n");
    }

    h_free_parser(parser);
    free(buffer);
    return 0;
}
