#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

static HParser *li_vn_mode_parser() {
    return h_bits(8, false);
}

static HParser *stratum_parser() {
    return h_bits(8, false);
}

static HParser *poll_parser() {
    return h_bits(8, false);
}

static HParser *precision_parser() {
    return h_bits(8, false);
}

static HParser *root_delay_parser() {
    return h_bits(32, false);
}

static HParser *root_dispersion_parser() {
    return h_bits(32, false);
}

static HParser *reference_id_parser() {
    return h_bits(32, false);
}

static HParser *timestamp_parser() {
    return h_bits(64, false);
}

static HParser *extension_field_parser() {
    return h_sequence(
        h_bits(16, false),
        h_bits(16, false),
        h_uint8(),
        NULL
    );
}

static HParser *extension_fields_parser() {
    return h_many(extension_field_parser());
}

static HParser *ntp_packet_parser() {
    return h_sequence(
        li_vn_mode_parser(),
        stratum_parser(),
        poll_parser(),
        precision_parser(),
        root_delay_parser(),
        root_dispersion_parser(),
        reference_id_parser(),
        timestamp_parser(),
        timestamp_parser(),
        timestamp_parser(),
        timestamp_parser(),
        extension_fields_parser(),
        NULL
    );
}

void print_parsed_result(const HParseResult *result) {
    if (!result || !result->ast) {
        printf("Failed to parse NTP packet\n");
        return;
    }

    const HParsedToken *token = result->ast;
    if (token->token_type != TT_SEQUENCE) {
        printf("Unexpected token type\n");
        return;
    }

    uint8_t li_vn_mode = token->seq->elements[0]->uint;
    uint8_t li = (li_vn_mode >> 6) & 0x03;
    uint8_t vn = (li_vn_mode >> 3) & 0x07;
    uint8_t mode = li_vn_mode & 0x07;

    printf("Leap Indicator: %u\n", li);
    printf("Version: %u\n", vn);
    printf("Mode: %u\n", mode);
    printf("Stratum: %u\n", token->seq->elements[1]->uint);
    printf("Poll: %u\n", token->seq->elements[2]->uint);
    printf("Precision: %u\n", token->seq->elements[3]->uint);
    printf("Root Delay: 0x%08x\n", token->seq->elements[4]->uint);
    printf("Root Dispersion: 0x%08x\n", token->seq->elements[5]->uint);
    printf("Reference ID: 0x%08x\n", token->seq->elements[6]->uint);
    printf("Reference Timestamp: 0x%016llx\n", (unsigned long long)token->seq->elements[7]->uint);
    printf("Origin Timestamp: 0x%016llx\n", (unsigned long long)token->seq->elements[8]->uint);
    printf("Receive Timestamp: 0x%016llx\n", (unsigned long long)token->seq->elements[9]->uint);
    printf("Transmit Timestamp: 0x%016llx\n", (unsigned long long)token->seq->elements[10]->uint);

    const HParsedToken *extensions = token->seq->elements[11];
    if (extensions && extensions->token_type == TT_SEQUENCE) {
        size_t num_extensions = extensions->seq->used;
        printf("Number of Extension Fields: %zu\n", num_extensions);
        
        for (size_t i = 0; i < num_extensions; i++) {
            const HParsedToken *ext = extensions->seq->elements[i];
            uint16_t type = ext->seq->elements[0]->uint;
            uint16_t length = ext->seq->elements[1]->uint;
            printf("Extension Field %zu: Type=0x%04x, Length=%u\n", i+1, type, length);
        }
    }
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <ntp_packet_file>\n", argv[0]);
        return 1;
    }

    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open input file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    long fsize = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t *buffer = malloc(fsize);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(f);
        return 1;
    }

    if (fread(buffer, 1, fsize, f) != (size_t)fsize) {
        perror("Failed to read file");
        free(buffer);
        fclose(f);
        return 1;
    }
    fclose(f);

    HParser *parser = ntp_packet_parser();
    HParseResult *result = h_parse(parser, buffer, fsize);
    
    print_parsed_result(result);

    if (result) {
        h_parse_result_free(result);
    }
    free(buffer);

    return 0;
}