#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// NITF Parser definitions
HParser* nitf_fhdr() {
    return h_token((const uint8_t*)"NITF", 4);
}

HParser* nitf_fver() {
    return h_token((const uint8_t*)"02.10", 5);
}

HParser* nitf_clevel() {
    return h_int_range(h_uint8(), 0, 99);
}

HParser* nitf_stype() {
    return h_token((const uint8_t*)"BF", 2);
}

HParser* nitf_ostaid() {
    return h_length_value(h_uint8(), h_repeat_n(h_ch_range(0x20, 0x7E), 10));
}

HParser* nitf_fdt() {
    HParser* digit = h_ch_range('0', '9');
    return h_sequence(
        h_repeat_n(digit, 4),  // CCYY
        h_repeat_n(digit, 2),  // MM
        h_repeat_n(digit, 2),  // DD
        h_repeat_n(digit, 2),  // hh
        h_repeat_n(digit, 2),  // mm
        h_repeat_n(digit, 2),  // ss
        NULL
    );
}

HParser* nitf_ftitle() {
    return h_length_value(h_uint8(), h_repeat_n(h_ch_range(0x20, 0x7E), 80));
}

HParser* nitf_fsclas() {
    return h_choice(h_ch('T'), h_ch('S'), h_ch('C'), h_ch('R'), h_ch('U'), NULL);
}

HParser* nitf_fscop() {
    return h_int_range(h_uint32(), 0, 99999);
}

HParser* nitf_fscpys() {
    return h_int_range(h_uint32(), 0, 99999);
}

// Image Segment Parser
HParser* nitf_im_segment() {
    return h_sequence(
        h_token((const uint8_t*)"IM", 2),
        h_length_value(h_uint32(), h_repeat_n(h_uint8(), 1)),
        NULL
    );
}

// Graphics Segment Parser
HParser* nitf_gr_segment() {
    return h_sequence(
        h_token((const uint8_t*)"GR", 2),
        h_length_value(h_uint32(), h_repeat_n(h_uint8(), 1)),
        NULL
    );
}

// Text Segment Parser
HParser* nitf_tx_segment() {
    return h_sequence(
        h_token((const uint8_t*)"TX", 2),
        h_length_value(h_uint32(), h_repeat_n(h_uint8(), 1)),
        NULL
    );
}

// Data Extension Segment Parser
HParser* nitf_de_segment() {
    return h_sequence(
        h_token((const uint8_t*)"DE", 2),
        h_length_value(h_uint32(), h_repeat_n(h_uint8(), 1)),
        NULL
    );
}

// Reserved Extension Segment Parser
HParser* nitf_re_segment() {
    return h_sequence(
        h_token((const uint8_t*)"RE", 2),
        h_length_value(h_uint32(), h_repeat_n(h_uint8(), 1)),
        NULL
    );
}

// Main NITF Parser
HParser* nitf_parser() {
    return h_sequence(
        nitf_fhdr(),
        nitf_fver(),
        nitf_clevel(),
        nitf_stype(),
        nitf_ostaid(),
        nitf_fdt(),
        nitf_ftitle(),
        nitf_fsclas(),
        nitf_fscop(),
        nitf_fscpys(),
        h_many(nitf_im_segment()),
        h_many(nitf_gr_segment()),
        h_many(nitf_tx_segment()),
        h_many(nitf_de_segment()),
        h_many(nitf_re_segment()),
        NULL
    );
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <nitf_file>\n", argv[0]);
        return 1;
    }

    FILE* fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("Failed to open file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t* input = malloc(size);
    if (!input) {
        perror("Failed to allocate memory");
        fclose(fp);
        return 1;
    }

    if (fread(input, 1, size, fp) != size) {
        perror("Failed to read file");
        free(input);
        fclose(fp);
        return 1;
    }

    HParser* parser = nitf_parser();
    HParseResult* result = h_parse(parser, input, size);

    if (result) {
        printf("Successfully parsed NITF file\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse NITF file\n");
    }

    free(input);
    fclose(fp);
    return 0;
}