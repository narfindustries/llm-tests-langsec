#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

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
    uint8_t* extension_fields;
    size_t extension_length;
    uint8_t* mac;
    size_t mac_length;
} ntp_packet;

static HParser* init_ntp_parser(void) {
    // Basic fields
    HParser* li_vn_mode = h_bits(8, false);
    HParser* stratum = h_bits(8, false);
    HParser* poll = h_bits(8, false);
    HParser* precision = h_bits(8, false);
    HParser* root_delay = h_bits(32, false);
    HParser* root_dispersion = h_bits(32, false);
    HParser* reference_id = h_bits(32, false);
    HParser* timestamp = h_bits(64, false);

    // Extension field
    HParser* extension_field_type = h_bits(16, false);
    HParser* extension_field_length = h_bits(16, false);
    HParser* extension_field_value = h_length_value(extension_field_length, h_uint8());
    HParser* extension_field = h_sequence(extension_field_type, extension_field_length, 
                                        extension_field_value, NULL);
    HParser* extension_fields = h_many(extension_field);

    // MAC field
    HParser* key_id = h_bits(32, false);
    HParser* message_digest = h_length_value(h_int_range(h_bits(16, false), 16, 20), h_uint8());
    HParser* mac = h_sequence(key_id, message_digest, NULL);

    // Complete NTP packet
    return h_sequence(li_vn_mode,
                     stratum,
                     poll,
                     precision,
                     root_delay,
                     root_dispersion,
                     reference_id,
                     timestamp,  // reference timestamp
                     timestamp,  // origin timestamp
                     timestamp,  // receive timestamp
                     timestamp,  // transmit timestamp
                     extension_fields,
                     h_optional(mac),
                     NULL);
}

static void print_ntp_packet(const HParsedToken* token) {
    if (!token) return;

    printf("LI_VN_MODE: 0x%02x\n", (uint8_t)token->seq->elements[0]->uint);
    printf("Stratum: %u\n", (uint8_t)token->seq->elements[1]->uint);
    printf("Poll: %d\n", (int8_t)token->seq->elements[2]->uint);
    printf("Precision: %d\n", (int8_t)token->seq->elements[3]->uint);
    printf("Root Delay: 0x%08x\n", (uint32_t)token->seq->elements[4]->uint);
    printf("Root Dispersion: 0x%08x\n", (uint32_t)token->seq->elements[5]->uint);
    printf("Reference ID: 0x%08x\n", (uint32_t)token->seq->elements[6]->uint);
    printf("Reference Timestamp: 0x%016lx\n", (uint64_t)token->seq->elements[7]->uint);
    printf("Origin Timestamp: 0x%016lx\n", (uint64_t)token->seq->elements[8]->uint);
    printf("Receive Timestamp: 0x%016lx\n", (uint64_t)token->seq->elements[9]->uint);
    printf("Transmit Timestamp: 0x%016lx\n", (uint64_t)token->seq->elements[10]->uint);
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
    fclose(f);

    HParser *ntp_parser = init_ntp_parser();
    if (!ntp_parser) {
        free(buf);
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }

    HParseResult *result = h_parse(ntp_parser, buf, size);
    if (!result) {
        free(buf);
        fprintf(stderr, "Parse failed\n");
        return 1;
    }

    print_ntp_packet(result->ast);

    h_parse_result_free(result);
    free(buf);
    return 0;
}