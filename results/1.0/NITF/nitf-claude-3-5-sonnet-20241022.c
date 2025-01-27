#include <hammer/hammer.h>
#include <stdio.h>

static const uint8_t tag_nitf[] = "NITF";
static const uint8_t tag_fhdr[] = "FHDR";
static const uint8_t tag_ver[] = "02.10";

HParser* init_nitf_parser() {
    // Basic building blocks
    HParser* space = h_in((const uint8_t*)" ", 1);
    HParser* digit = h_ch_range(0x30, 0x39);
    HParser* alpha = h_choice(h_ch_range(0x41, 0x5A), h_ch_range(0x61, 0x7A), NULL);
    HParser* alphanumeric = h_choice(alpha, digit, NULL);

    // Common elements
    HParser* nitf_tag = h_token((const uint8_t*)"NITF", 4);
    HParser* fhdr_tag = h_token((const uint8_t*)"FHDR", 4);
    HParser* ver_tag = h_token((const uint8_t*)"02.10", 5);

    // Field lengths and types
    HParser* length_2 = h_repeat_n(digit, 2);
    HParser* length_4 = h_repeat_n(digit, 4);
    HParser* length_5 = h_repeat_n(digit, 5);
    HParser* length_7 = h_repeat_n(digit, 7);
    HParser* length_10 = h_repeat_n(digit, 10);
    HParser* length_14 = h_repeat_n(digit, 14);

    // Date/time fields
    HParser* date_time = h_repeat_n(digit, 14);  // CCYYMMDDhhmmss
    
    // Security fields
    HParser* security_field = h_repeat_n(alphanumeric, 2);
    
    // File structure
    HParser* file_header = h_sequence(
        nitf_tag,
        fhdr_tag,
        ver_tag,
        h_repeat_n(alphanumeric, 2),  // CLEVEL
        h_repeat_n(alphanumeric, 4),  // STYPE
        h_repeat_n(alphanumeric, 10), // OSTAID
        date_time,                    // FDT
        h_repeat_n(alphanumeric, 14), // FTITLE
        security_field,               // Classification
        h_repeat_n(alphanumeric, 11), // FSCOP/FSCPYS
        h_repeat_n(alphanumeric, 2),  // ENCRYP
        h_repeat_n(alphanumeric, 3),  // FBKGC
        h_repeat_n(digit, 3),         // ONAME length
        h_repeat_n(digit, 2),         // OPHONE length
        NULL
    );

    return h_sequence(file_header, NULL);
}

int main(int argc, char* argv[]) {
    HParser* nitf_parser = init_nitf_parser();

    if (!nitf_parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }

    return 0;
}