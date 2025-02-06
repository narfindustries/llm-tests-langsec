#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    HParsedToken* file_header;
    HParsedToken* file_version;
    int64_t complexity_level;
    HParsedToken* security_type;
    HParsedToken* image_segments;
} NitfFile;

static HParser* nitf_file_header() {
    return h_sequence(
        h_literal("NITF"),
        h_literal("02.10"),
        h_int_range(h_int(10), 1, 9),
        h_choice(
            h_literal("TOP SECRET"),
            h_literal("SECRET"),
            h_literal("CONFIDENTIAL"),
            NULL
        )
    );
}

static HParser* nitf_image_segment() {
    return h_sequence(
        h_literal("IM"),
        h_choice(
            h_literal("VISUAL"),
            h_literal("INFRARED"),
            NULL
        ),
        h_choice(
            h_literal("B"),
            h_literal("P"),
            h_literal("R"),
            NULL
        )
    );
}

static HParser* nitf_parser() {
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
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("File read error");
        free(buffer);
        fclose(file);
        return 1;
    }

    HParser* parser = nitf_parser();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result && result->ast) {
        printf("NITF file parsed successfully\n");
    } else {
        printf("NITF file parsing failed\n");
    }

    h_parse_result_free(result);
    h_arena_free(parser->arena);
    free(buffer);
    fclose(file);

    return 0;
}