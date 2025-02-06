#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    char* fhdr;
    uint8_t clevel;
    char stype;
    char* ostaid;
    char* fdt;
} NitfFileHeader;

typedef struct {
    char* im;
    char* iloc;
    uint32_t nrows;
    uint32_t ncols;
    char* pvtype;
    char* irep;
} NitfImageSegment;

HParser* create_nitf_file_header_parser() {
    HParser* fhdr_parser = h_token((const uint8_t*)"NITF02.50", 9);
    HParser* clevel_parser = h_int_range(h_token((const uint8_t*)"1", 1), 1, 9);
    HParser* stype_parser = h_choice(
        h_token((const uint8_t*)"T", 1), 
        h_token((const uint8_t*)"S", 1), 
        h_token((const uint8_t*)"C", 1), 
        NULL
    );
    HParser* alpha_digit = h_choice(
        h_ch_range('A', 'Z'),
        h_ch_range('0', '9'),
        NULL
    );
    HParser* ostaid_parser = h_repeat_n(alpha_digit, 1);
    HParser* fdt_parser = h_repeat_n(h_ch_range('0', '9'), 14);

    return h_sequence(
        fhdr_parser,
        clevel_parser,
        stype_parser,
        ostaid_parser,
        fdt_parser,
        NULL
    );
}

HParser* create_image_segment_parser() {
    HParser* im_parser = h_repeat_n(h_ch_range('A', 'Z'), 1);
    HParser* iloc_parser = h_repeat_n(h_ch_range('0', '9'), 1);
    HParser* nrows_parser = h_int_range(h_token((const uint8_t*)"1", 1), 1, 100000);
    HParser* ncols_parser = h_int_range(h_token((const uint8_t*)"1", 1), 1, 100000);
    HParser* pvtype_parser = h_choice(
        h_token((const uint8_t*)"INT", 3),
        h_token((const uint8_t*)"FLOAT", 5),
        NULL
    );
    HParser* irep_parser = h_repeat_n(h_ch_range('A', 'Z'), 1);

    return h_sequence(
        im_parser,
        iloc_parser,
        nrows_parser,
        ncols_parser,
        pvtype_parser,
        irep_parser,
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

    HParser* nitf_parser = h_sequence(
        create_nitf_file_header_parser(),
        create_image_segment_parser(),
        NULL
    );

    HParseResult* result = h_parse(nitf_parser, buffer, file_size);
    if (!result) {
        fprintf(stderr, "Parsing failed\n");
        free(buffer);
        fclose(file);
        return 1;
    }

    h_parse_result_free(result);
    free(buffer);
    fclose(file);
    return 0;
}