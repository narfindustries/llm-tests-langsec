#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NITF_HEADER_SIZE 500

HParser *nitf_parser() {
    HParser *whitespace = h_whitespace(h_epsilon_p());
    HParser *header_parser = h_sequence(
        h_length_value(h_uint8(), whitespace), // FHDR
        h_length_value(h_uint8(), whitespace), // FVER
        h_length_value(h_uint8(), whitespace), // CLEVEL
        h_length_value(h_uint8(), whitespace), // STYPE
        h_length_value(h_uint8(), whitespace), // OSTAID
        h_length_value(h_uint8(), whitespace), // FDT
        h_length_value(h_uint8(), whitespace), // FTITLE
        h_length_value(h_uint8(), whitespace), // FSCLAS
        h_length_value(h_uint8(), whitespace), // FSCODE
        h_length_value(h_uint8(), whitespace), // FSCTLH
        h_length_value(h_uint8(), whitespace), // FSREL
        h_length_value(h_uint8(), whitespace), // FSDCTP
        h_length_value(h_uint8(), whitespace), // FSDCDT
        h_length_value(h_uint8(), whitespace), // FSDCXM
        h_length_value(h_uint8(), whitespace), // FSDG
        h_length_value(h_uint8(), whitespace), // FSDGDT
        h_length_value(h_uint8(), whitespace), // FSCLTX
        h_length_value(h_uint8(), whitespace), // FSCATP
        h_length_value(h_uint8(), whitespace), // FSCAUT
        h_length_value(h_uint8(), whitespace), // FSCRSN
        h_length_value(h_uint8(), whitespace), // FSSRDT
        h_length_value(h_uint8(), whitespace), // FSCTLN
        h_length_value(h_uint8(), whitespace), // FSCOP
        h_length_value(h_uint8(), whitespace), // FSCPYS
        h_length_value(h_uint8(), whitespace), // ONAME
        h_length_value(h_uint8(), whitespace), // OPHONE
        NULL
    );

    return header_parser;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    uint8_t buffer[NITF_HEADER_SIZE];
    size_t bytes_read = fread(buffer, 1, NITF_HEADER_SIZE, file);
    fclose(file);

    HParser *parser = nitf_parser();
    HParseResult *result = h_parse(parser, buffer, bytes_read);

    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }

    return 0;
}