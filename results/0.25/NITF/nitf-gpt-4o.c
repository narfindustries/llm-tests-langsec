#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Define parsers for various fields in the NITF specification
HParser *create_nitf_parser() {
    // Basic parsers for common field types
    HParser *alpha = h_ch_range('A', 'Z');
    HParser *digit = h_ch_range('0', '9');
    HParser *alnum = h_choice(alpha, digit, NULL);
    HParser *space = h_ch(' ');

    // Parsers for specific fields
    HParser *fhdr = h_sequence(h_ch('N'), h_ch('I'), h_ch('T'), h_ch('F'), NULL);
    HParser *clevel = h_repeat_n(digit, 2);
    HParser *stype = h_sequence(h_ch('B'), h_ch('F'), h_repeat_n(digit, 2), NULL);
    HParser *ostaid = h_repeat_n(alnum, 10);
    HParser *fdt = h_repeat_n(digit, 14);
    HParser *ftitle = h_repeat_n(alnum, 80);
    HParser *fsclas = h_choice(h_ch('U'), h_ch('C'), h_ch('S'), h_ch('T'), h_ch('R'), NULL);
    HParser *fsclsy = h_repeat_n(alnum, 2);
    HParser *fscode = h_repeat_n(alnum, 11);
    HParser *fsctlh = h_repeat_n(alnum, 2);
    HParser *fsrel = h_repeat_n(alnum, 20);
    HParser *fsdctp = h_choice(h_ch('D'), h_ch('E'), h_ch('X'), h_ch('O'), NULL);
    HParser *fsdcdt = h_repeat_n(digit, 8);
    HParser *fsdcxm = h_repeat_n(alnum, 4);
    HParser *fsdg = h_choice(h_ch('S'), h_ch('C'), h_ch('R'), h_ch('U'), NULL);
    HParser *fsdgdt = h_repeat_n(digit, 8);
    HParser *fscltx = h_repeat_n(alnum, 43);
    HParser *fscatp = h_choice(h_ch('O'), h_ch('D'), NULL);
    HParser *fscaut = h_repeat_n(alnum, 40);
    HParser *fscrsn = h_repeat_n(alnum, 1);
    HParser *fssrdt = h_repeat_n(digit, 8);
    HParser *fsctln = h_repeat_n(alnum, 15);
    HParser *fscop = h_repeat_n(digit, 5);
    HParser *fscpys = h_repeat_n(digit, 5);
    HParser *encryp = h_choice(h_ch('0'), h_ch('1'), NULL);

    // Combine all parsers into a single parser for the NITF file header
    HParser *nitf_header = h_sequence(
        fhdr, clevel, stype, ostaid, fdt, ftitle, fsclas, fsclsy, fscode, fsctlh,
        fsrel, fsdctp, fsdcdt, fsdcxm, fsdg, fsdgdt, fscltx, fscatp, fscaut,
        fscrsn, fssrdt, fsctln, fscop, fscpys, encryp, NULL
    );

    return nitf_header;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <nitf_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    // Open the file specified in the command line argument
    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    // Determine the file size
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    // Read the file into a buffer
    unsigned char *buffer = malloc(file_size);
    if (!buffer) {
        perror("Error allocating memory");
        fclose(file);
        return EXIT_FAILURE;
    }
    fread(buffer, 1, file_size, file);
    fclose(file);

    // Create the NITF parser
    HParser *nitf_parser = create_nitf_parser();

    // Parse the buffer
    HParseResult *result = h_parse(nitf_parser, buffer, file_size);
    if (result) {
        printf("NITF file parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse NITF file.\n");
    }

    // Clean up
    free(buffer);
    h_parser_free(nitf_parser);

    return EXIT_SUCCESS;
}