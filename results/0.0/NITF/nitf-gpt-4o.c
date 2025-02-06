#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Define parsers for NITF fields
HParser *parse_fhdr() {
    return h_sequence(h_ch('N'), h_ch('I'), h_ch('T'), h_ch('F'), h_ch('0'), h_ch('2'), h_ch('.'), h_ch('1'), h_ch('0'), NULL);
}

HParser *parse_clevel() {
    return h_repeat_n(h_ch_range('0', '9'), 2);
}

HParser *parse_stype() {
    return h_sequence(h_ch('B'), h_ch('F'), h_ch('0'), h_ch('1'), NULL);
}

HParser *parse_ostaid() {
    return h_repeat_n(h_ch_range('A', 'Z'), 10);
}

HParser *parse_fdt() {
    return h_repeat_n(h_ch_range('0', '9'), 14);
}

HParser *parse_ftitle() {
    return h_repeat_n(h_ch_range(' ', '~'), 80);
}

HParser *parse_fsclas() {
    return h_choice(h_ch('U'), h_ch('C'), h_ch('S'), h_ch('T'), NULL);
}

HParser *parse_fscode() {
    return h_repeat_n(h_ch_range(' ', '~'), 40);
}

HParser *parse_fsctlh() {
    return h_repeat_n(h_ch_range(' ', '~'), 40);
}

HParser *parse_fsrel() {
    return h_repeat_n(h_ch_range(' ', '~'), 40);
}

HParser *parse_fsdctp() {
    return h_repeat_n(h_ch_range(' ', '~'), 1);
}

HParser *parse_fsdcdt() {
    return h_repeat_n(h_ch_range('0', '9'), 8);
}

HParser *parse_fsdcxm() {
    return h_repeat_n(h_ch_range(' ', '~'), 40);
}

HParser *parse_fscop() {
    return h_repeat_n(h_ch_range('0', '9'), 5);
}

HParser *parse_fscpys() {
    return h_repeat_n(h_ch_range('0', '9'), 5);
}

HParser *parse_encryp() {
    return h_choice(h_ch('0'), h_ch('1'), NULL);
}

// Define the NITF file header parser
HParser *parse_nitf_header() {
    return h_sequence(
        parse_fhdr(),
        parse_clevel(),
        parse_stype(),
        parse_ostaid(),
        parse_fdt(),
        parse_ftitle(),
        parse_fsclas(),
        parse_fscode(),
        parse_fsctlh(),
        parse_fsrel(),
        parse_fsdctp(),
        parse_fsdcdt(),
        parse_fsdcxm(),
        parse_fscop(),
        parse_fscpys(),
        parse_encryp(),
        NULL
    );
}

// Main function to parse a binary file
int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *nitf_parser = parse_nitf_header();
    HParseResult *result = h_parse(nitf_parser, data, file_size);

    if (result) {
        printf("NITF file parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse NITF file.\n");
    }

    free(data);
    return EXIT_SUCCESS;
}