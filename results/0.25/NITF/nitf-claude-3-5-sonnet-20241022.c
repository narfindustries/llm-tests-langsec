#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
    return h_length_value(h_uint8(), h_uint8());
}

HParser* nitf_fdt() {
    return h_length_value(h_uint8(), h_uint8());
}

HParser* nitf_ftitle() {
    return h_length_value(h_uint8(), h_uint8());
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

HParser* nitf_encryp() {
    return h_choice(h_ch('0'), h_ch('1'), NULL);
}

// Image Segment
HParser* nitf_im() {
    return h_sequence(
        h_token((const uint8_t*)"IM", 2),
        h_length_value(h_uint32(), h_uint8()),
        NULL
    );
}

// Graphics Segment
HParser* nitf_gs() {
    return h_sequence(
        h_token((const uint8_t*)"GS", 2),
        h_length_value(h_uint32(), h_uint8()),
        NULL
    );
}

// Text Segment
HParser* nitf_ts() {
    return h_sequence(
        h_token((const uint8_t*)"TS", 2),
        h_length_value(h_uint32(), h_uint8()),
        NULL
    );
}

// Data Extension Segment
HParser* nitf_des() {
    return h_sequence(
        h_token((const uint8_t*)"DE", 2),
        h_length_value(h_uint32(), h_uint8()),
        NULL
    );
}

// Reserved Extension Segment
HParser* nitf_res() {
    return h_sequence(
        h_token((const uint8_t*)"RE", 2),
        h_length_value(h_uint32(), h_uint8()),
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
        nitf_encryp(),
        h_many(nitf_im()),
        h_many(nitf_gs()),
        h_many(nitf_ts()),
        h_many(nitf_des()),
        h_many(nitf_res()),
        NULL
    );
}

int main(int argc, char* argv[]) {
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

    uint8_t* buffer = malloc(size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(fp);
        return 1;
    }

    if (fread(buffer, 1, size, fp) != size) {
        perror("Failed to read file");
        free(buffer);
        fclose(fp);
        return 1;
    }

    HParser* parser = nitf_parser();
    HParseResult* result = h_parse(parser, buffer, size);

    if (result) {
        printf("Successfully parsed NITF file\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse NITF file\n");
    }

    free(buffer);
    fclose(fp);
    return 0;
}