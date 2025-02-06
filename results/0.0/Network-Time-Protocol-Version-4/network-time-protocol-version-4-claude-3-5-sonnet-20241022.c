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

static void print_token(const HParsedToken *token) {
    if (!token) return;

    switch (token->token_type) {
        case TT_SEQUENCE:
            for (size_t i = 0; i < token->seq->used; i++) {
                print_token(token->seq->elements[i]);
            }
            break;
        case TT_UINT:
            printf("0x%llx\n", (unsigned long long)token->uint);
            break;
        case TT_BYTES:
            for (size_t i = 0; i < token->bytes.len; i++) {
                printf("%02x", token->bytes.token[i]);
            }
            printf("\n");
            break;
        default:
            break;
    }
}

static HParser* create_ntp_parser(void) {
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
    HParser *extension_value = h_length_value(h_uint16(), h_uint8());
    HParser *extension_field = h_sequence(h_uint16(), h_uint16(), extension_value, NULL);
    
    // MAC field parser
    HParser *mac_field = h_sequence(h_uint32(), h_many1(h_uint8()), NULL);

    // Optional fields
    HParser *optional_extensions = h_many(extension_field);
    HParser *optional_mac = h_optional(mac_field);

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
        optional_extensions,
        optional_mac,
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

    uint8_t *data = malloc(size);
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
    fclose(f);

    HParser *ntp_parser = create_ntp_parser();
    if (!ntp_parser) {
        free(data);
        fprintf(stderr, "Failed to create parser\n");
        return 1;
    }

    HParseResult *result = h_parse(ntp_parser, data, size);
    if (!result) {
        free(data);
        fprintf(stderr, "Parse failed\n");
        return 1;
    }

    printf("NTP Packet Contents:\n");
    print_token(result->ast);

    h_parse_result_free(result);
    free(data);
    return 0;
}