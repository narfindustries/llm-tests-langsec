#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Define parsers for the NITF fields
HParser *parse_string(int length) {
    return h_repeat_n(h_ch_range(0x20, 0x7E), length);
}

HParser *parse_date_time() {
    return h_sequence(parse_string(14), NULL); // YYYYMMDDhhmmss format
}

HParser *parse_classification() {
    return h_choice(
        h_token("U", 1), // Unclassified
        h_token("C", 1), // Confidential
        h_token("S", 1), // Secret
        h_token("TS", 2), // Top Secret
        NULL
    );
}

// Define the parser for the NITF file header
HParser *nitf_file_header() {
    return h_sequence(
        parse_string(9),   // FHDR
        parse_string(2),   // FVER
        parse_string(2),   // CLEVEL
        parse_string(10),  // STYPE
        parse_string(10),  // OSTAID
        parse_date_time(), // FDT
        parse_string(80),  // FTITLE
        h_repeat_n(h_uint8(), 24), // FSCOP, FSCPYS, ENCRYP, FBKGC
        parse_classification(),    // FSCLAS
        parse_string(2),           // FSCLSY
        parse_string(11),          // FSCODE
        NULL
    );
}

// Define the main function
int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("fopen");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *data = (unsigned char *)malloc(length);
    if (!data) {
        perror("malloc");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(data, 1, length, file);
    fclose(file);

    HParser *parser = nitf_file_header();
    HParseResult *result = h_parse(parser, data, length);

    if (result != NULL && result->ast != NULL) {
        printf("NITF file parsed successfully.\n");
    } else {
        fprintf(stderr, "Failed to parse NITF file.\n");
    }

    h_parse_result_free(result);
    free(data);
    return EXIT_SUCCESS;
}