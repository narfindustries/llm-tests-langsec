#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    char fhdr[10];
    char clevel[3];
    char stype;
    char ostaid[11];
    size_t fl;
    char encryp;
} NitfHeader;

typedef struct {
    char im[11];
    char isorce[43];
    size_t nrows;
    size_t ncols;
    char pvtype[3];
    char imode;
} NitfImageSubheader;

HParser* nitf_header_parser() {
    return h_sequence(
        h_token("NITF02.10", 9),
        h_int_range(h_int(), 0, 9),
        h_choice(
            h_ch('R'),
            h_ch('C'),
            h_ch('S'),
            h_ch('T')
        ),
        h_repeat_n(h_ch_range('A', 'Z'), 10),
        h_int64(),
        h_choice(
            h_ch('0'),
            h_ch('1')
        ),
        NULL
    );
}

HParser* nitf_image_subheader_parser() {
    return h_sequence(
        h_repeat_n(h_ch_range('A', 'Z'), 10),
        h_repeat_n(h_ch_range('A', 'Z'), 42),
        h_int64(),
        h_int64(),
        h_choice(
            h_token("INT", 3),
            h_ch('B'),
            h_token("SI", 2),
            h_ch('R'),
            h_ch('C')
        ),
        h_choice(
            h_ch('B'),
            h_ch('P'),
            h_ch('R'),
            h_ch('S')
        ),
        NULL
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

    HParser* header_parser = nitf_header_parser();
    HParser* image_parser = nitf_image_subheader_parser();

    HParseResult* header_result = h_parse(header_parser, buffer, file_size);
    HParseResult* image_result = h_parse(image_parser, buffer, file_size);

    if (header_result && image_result) {
        printf("NITF file parsed successfully\n");
    } else {
        printf("NITF file parsing failed\n");
    }

    h_parse_result_free(header_result);
    h_parse_result_free(image_result);
    h_destroy_parser(header_parser);
    h_destroy_parser(image_parser);

    free(buffer);
    fclose(file);

    return 0;
}