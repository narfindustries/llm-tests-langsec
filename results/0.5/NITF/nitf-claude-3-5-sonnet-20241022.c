#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// NITF File Header parsers
HParser* nitf_fhdr() {
    return h_token((const uint8_t*)"NITF", 4);
}

HParser* nitf_fver() {
    return h_token((const uint8_t*)"02.10", 5);
}

HParser* nitf_clevel() {
    return h_int_range(h_uint8(), 3, 6);
}

HParser* nitf_stype() {
    return h_token((const uint8_t*)"BF", 2);
}

HParser* nitf_ostaid() {
    return h_repeat_n(h_ch_range(0x20, 0x7E), 10);
}

HParser* nitf_fdt() {
    return h_repeat_n(h_ch_range(0x20, 0x7E), 14);
}

HParser* nitf_ftitle() {
    return h_repeat_n(h_ch_range(0x20, 0x7E), 80);
}

HParser* nitf_fsclas() {
    return h_choice(h_ch('T'), h_ch('S'), h_ch('C'), h_ch('R'), h_ch('U'), NULL);
}

HParser* nitf_fscode() {
    return h_repeat_n(h_ch_range(0x20, 0x7E), 40);
}

HParser* nitf_fsctlh() {
    return h_repeat_n(h_ch_range(0x20, 0x7E), 40);
}

HParser* nitf_fsrel() {
    return h_repeat_n(h_ch_range(0x20, 0x7E), 40);
}

HParser* nitf_fscaut() {
    return h_repeat_n(h_ch_range(0x20, 0x7E), 20);
}

HParser* nitf_fsctln() {
    return h_repeat_n(h_ch_range(0x20, 0x7E), 20);
}

HParser* nitf_fscop() {
    return h_repeat_n(h_ch_range(0x30, 0x39), 5);
}

HParser* nitf_fscpys() {
    return h_repeat_n(h_ch_range(0x30, 0x39), 5);
}

HParser* nitf_encryp() {
    return h_ch('0');
}

HParser* nitf_fbkgc() {
    return h_repeat_n(h_uint8(), 3);
}

HParser* nitf_oname() {
    return h_repeat_n(h_ch_range(0x20, 0x7E), 24);
}

HParser* nitf_ophone() {
    return h_repeat_n(h_ch_range(0x20, 0x7E), 18);
}

// Image Segment parsers
HParser* nitf_im() {
    return h_sequence(
        h_token((const uint8_t*)"IM", 2),
        h_uint16(),  // IID1
        h_uint16(),  // IDATIM
        h_uint16(),  // TGTID
        h_uint16(),  // IID2
        h_uint8(),   // ISCLAS
        h_repeat_n(h_ch_range(0x20, 0x7E), 40),  // ISCLSY
        h_repeat_n(h_ch_range(0x20, 0x7E), 40),  // ISCODE
        h_repeat_n(h_ch_range(0x20, 0x7E), 40),  // ISCTLH
        h_repeat_n(h_ch_range(0x20, 0x7E), 40),  // ISREL
        h_repeat_n(h_ch_range(0x20, 0x7E), 20),  // ISDCTP
        h_repeat_n(h_ch_range(0x20, 0x7E), 40),  // ISDCDT
        h_repeat_n(h_ch_range(0x20, 0x7E), 40),  // ISDCXM
        h_repeat_n(h_ch_range(0x20, 0x7E), 20),  // ISDG
        h_repeat_n(h_ch_range(0x20, 0x7E), 20),  // ISDGDT
        h_repeat_n(h_ch_range(0x20, 0x7E), 40),  // ISCLTX
        h_repeat_n(h_ch_range(0x20, 0x7E), 40),  // ISCATP
        h_repeat_n(h_ch_range(0x20, 0x7E), 20),  // ISCAUT
        h_repeat_n(h_ch_range(0x20, 0x7E), 20),  // ISCRSN
        h_repeat_n(h_ch_range(0x20, 0x7E), 20),  // ISSRDT
        h_repeat_n(h_ch_range(0x20, 0x7E), 200), // ISCTLN
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
        nitf_fscode(),
        nitf_fsctlh(),
        nitf_fsrel(),
        nitf_fscaut(),
        nitf_fsctln(),
        nitf_fscop(),
        nitf_fscpys(),
        nitf_encryp(),
        nitf_fbkgc(),
        nitf_oname(),
        nitf_ophone(),
        h_many(nitf_im()),
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

    if (!result) {
        fprintf(stderr, "Failed to parse NITF file\n");
        free(buffer);
        fclose(fp);
        return 1;
    }

    h_parse_result_free(result);
    free(buffer);
    fclose(fp);
    return 0;
}