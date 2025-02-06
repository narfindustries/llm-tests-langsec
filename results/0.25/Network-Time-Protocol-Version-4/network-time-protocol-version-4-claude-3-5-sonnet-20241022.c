#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    uint8_t li_vn_mode;
    uint8_t stratum;
    uint8_t poll;
    uint8_t precision;
    uint32_t root_delay;
    uint32_t root_dispersion;
    uint32_t reference_id;
    uint64_t reference_timestamp;
    uint64_t origin_timestamp;
    uint64_t receive_timestamp;
    uint64_t transmit_timestamp;
} ntp_packet;

typedef struct {
    uint16_t field_type;
    uint16_t length;
    uint8_t *value;
} extension_field;

typedef struct {
    uint32_t key_id;
    uint8_t *digest;
} mac_field;

static void print_ntp_packet(const HParseResult *result) {
    if (!result || !result->ast) return;

    const HParsedToken *token = result->ast;
    if (token->token_type != TT_SEQUENCE) return;

    printf("LI_VN_MODE: 0x%02x\n", (uint8_t)token->seq->elements[0]->uint);
    printf("Stratum: %u\n", (uint8_t)token->seq->elements[1]->uint);
    printf("Poll: %d\n", (int8_t)token->seq->elements[2]->uint);
    printf("Precision: %d\n", (int8_t)token->seq->elements[3]->uint);
    printf("Root Delay: 0x%08x\n", (uint32_t)token->seq->elements[4]->uint);
    printf("Root Dispersion: 0x%08x\n", (uint32_t)token->seq->elements[5]->uint);
    printf("Reference ID: 0x%08x\n", (uint32_t)token->seq->elements[6]->uint);
    printf("Reference Timestamp: 0x%016llx\n", (unsigned long long)token->seq->elements[7]->uint);
    printf("Origin Timestamp: 0x%016llx\n", (unsigned long long)token->seq->elements[8]->uint);
    printf("Receive Timestamp: 0x%016llx\n", (unsigned long long)token->seq->elements[9]->uint);
    printf("Transmit Timestamp: 0x%016llx\n", (unsigned long long)token->seq->elements[10]->uint);

    if (token->seq->elements[11] && token->seq->elements[11]->token_type != TT_NONE) {
        printf("Optional Fields Present\n");
    }
}

static HParser *create_ntp_parser(void) {
    // Basic fields
    HParser *li_vn_mode = h_uint8();
    HParser *stratum = h_uint8();
    HParser *poll = h_uint8();
    HParser *precision = h_uint8();
    HParser *root_delay = h_uint32();
    HParser *root_dispersion = h_uint32();
    HParser *reference_id = h_uint32();
    HParser *reference_timestamp = h_uint64();
    HParser *origin_timestamp = h_uint64();
    HParser *receive_timestamp = h_uint64();
    HParser *transmit_timestamp = h_uint64();

    // Extension field parser
    HParser *extension_field = h_sequence(
        h_uint16(), // field type
        h_uint16(), // length
        h_length_value(h_uint16(), h_uint8()), // value with padding
        NULL
    );

    // MAC field parser
    HParser *mac_field = h_sequence(
        h_uint32(), // key id
        h_length_value(h_uint16(), h_uint8()), // digest
        NULL
    );

    // Optional fields
    HParser *optional_fields = h_many(h_choice(extension_field, mac_field, NULL));

    // Complete NTP packet
    return h_sequence(
        li_vn_mode,
        stratum,
        poll,
        precision,
        root_delay,
        root_dispersion,
        reference_id,
        reference_timestamp,
        origin_timestamp,
        receive_timestamp,
        transmit_timestamp,
        optional_fields,
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <ntp_packet_file>\n", argv[0]);
        return 1;
    }

    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t *buf = malloc(size);
    if (!buf) {
        fclose(f);
        fprintf(stderr, "Memory allocation failed\n");
        return 1;
    }

    if (fread(buf, 1, size, f) != size) {
        free(buf);
        fclose(f);
        fprintf(stderr, "Failed to read file\n");
        return 1;
    }

    HParser *ntp_parser = create_ntp_parser();
    if (!ntp_parser) {
        free(buf);
        fclose(f);
        fprintf(stderr, "Failed to create parser\n");
        return 1;
    }

    HParseResult *result = h_parse(ntp_parser, buf, size);
    if (result) {
        print_ntp_packet(result);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Parsing failed\n");
    }

    free(buf);
    fclose(f);
    return 0;
}