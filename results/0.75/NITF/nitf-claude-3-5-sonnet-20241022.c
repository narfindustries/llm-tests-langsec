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
    return h_int_range(h_uint8(), 1, 99);
}

HParser* nitf_stype() {
    return h_token((const uint8_t*)"BF", 2);
}

HParser* nitf_ostaid() {
    return h_length_value(h_uint8(), h_repeat_n(h_ch_range(0x20, 0x7E), 10));
}

HParser* nitf_fdt() {
    return h_token((const uint8_t*)"CCYYMMDDhhmmss", 14);
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

// Image segment parser
HParser* nitf_im() {
    return h_sequence(
        h_token((const uint8_t*)"IM", 2),
        h_int_range(h_uint16(), 0, 9999),
        h_repeat_n(h_ch_range(0x20, 0x7E), 80),
        NULL
    );
}

// Graphics segment parser
HParser* nitf_graphics() {
    return h_sequence(
        h_token((const uint8_t*)"GR", 2),
        h_int_range(h_uint16(), 0, 9999),
        h_repeat_n(h_ch_range(0x20, 0x7E), 80),
        NULL
    );
}

// Text segment parser
HParser* nitf_text() {
    return h_sequence(
        h_token((const uint8_t*)"TX", 2),
        h_int_range(h_uint16(), 0, 9999),
        h_repeat_n(h_ch_range(0x20, 0x7E), 80),
        NULL
    );
}

// Main NITF parser
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
        h_many(nitf_im()),
        h_many(nitf_graphics()),
        h_many(nitf_text()),
        NULL
    );
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <nitf_file>\n", argv[0]);
        return 1;
    }

    FILE* f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t* data = malloc(size);
    if (!data) {
        fclose(f);
        fprintf(stderr, "Memory allocation failed\n");
        return 1;
    }

    if (fread(data, 1, size, f) != size) {
        free(data);
        fclose(f);
        fprintf(stderr, "Failed to read file\n");
        return 1;
    }

    HParser* parser = nitf_parser();
    HParseResult* result = h_parse(parser, data, size);

    if (!result) {
        fprintf(stderr, "Parse failed\n");
        free(data);
        fclose(f);
        return 1;
    }

    h_parse_result_free(result);
    free(data);
    fclose(f);
    return 0;
}