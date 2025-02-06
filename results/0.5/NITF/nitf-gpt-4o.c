#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Define parsers for various fields based on NITF specifications
HParser *parse_fhdr() { return h_many1(h_ch_range('A', 'Z')); }
HParser *parse_clevel() { return h_many1(h_ch_range('0', '9')); }
HParser *parse_stype() { return h_many1(h_ch_range('A', 'Z')); }
HParser *parse_ostaid() { return h_many1(h_ch_range('A', 'Z')); }
HParser *parse_fdt() { return h_many1(h_ch_range('0', '9')); }
HParser *parse_ftitle() { return h_many1(h_ch_range(' ', '~')); }
HParser *parse_fsclas() { return h_choice(h_ch('U'), h_ch('C'), h_ch('S'), h_ch('T'), h_ch('R'), NULL); }
HParser *parse_fscode() { return h_many(h_ch_range('A', 'Z')); }
HParser *parse_fsctlh() { return h_many(h_ch_range('A', 'Z')); }
HParser *parse_fsrel() { return h_many(h_ch_range('A', 'Z')); }
HParser *parse_fsdctp() { return h_ch_range('A', 'Z'); }
HParser *parse_fsdcdt() { return h_many1(h_ch_range('0', '9')); }
HParser *parse_fscltx() { return h_many(h_ch_range(' ', '~')); }
HParser *parse_fscatp() { return h_choice(h_ch('O'), h_ch('D'), h_ch('M'), NULL); }
HParser *parse_fscaut() { return h_many(h_ch_range('A', 'Z')); }
HParser *parse_fscrsn() { return h_ch_range('A', 'Z'); }
HParser *parse_fssrdt() { return h_many1(h_ch_range('0', '9')); }
HParser *parse_fsctln() { return h_many(h_ch_range('0', '9')); }
HParser *parse_fsdg() { return h_many(h_ch_range('A', 'Z')); }
HParser *parse_fsdgdt() { return h_many1(h_ch_range('0', '9')); }

// Define a parser for the NITF file header
HParser *parse_nitf_header() {
    return h_sequence(
        parse_fhdr(), parse_clevel(), parse_stype(), parse_ostaid(), parse_fdt(),
        parse_ftitle(), parse_fsclas(), parse_fscode(), parse_fsctlh(), parse_fsrel(),
        parse_fsdctp(), parse_fsdcdt(), parse_fscltx(), parse_fscatp(), parse_fscaut(),
        parse_fscrsn(), parse_fssrdt(), parse_fsctln(), parse_fsdg(), parse_fsdgdt(),
        NULL
    );
}

void parse_nitf_file(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *header_parser = parse_nitf_header();
    HParseResult *result = h_parse(header_parser, buffer, file_size);
    if (result) {
        printf("NITF file parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse NITF file.\n");
    }

    free(buffer);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <nitf_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    parse_nitf_file(argv[1]);
    return EXIT_SUCCESS;
}