#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    uint8_t li_vn_mode;
    uint8_t stratum;
    uint8_t poll;
    int8_t precision;
    uint32_t root_delay;
    uint32_t root_dispersion;
    uint32_t reference_id;
    uint64_t reference_timestamp;
    uint64_t originate_timestamp;
    uint64_t receive_timestamp;
    uint64_t transmit_timestamp;
    uint32_t key_identifier;
    uint8_t message_digest[16];
    int has_auth;
} ntp_packet;

HParser *ntp_parser(void) {
    HParser *li_vn_mode = h_bits(8, false);
    HParser *stratum = h_uint8();
    HParser *poll = h_uint8();
    HParser *precision = h_int8();
    HParser *root_delay = h_uint32();
    HParser *root_dispersion = h_uint32();
    HParser *reference_id = h_uint32();
    HParser *timestamp = h_uint64();
    HParser *key_identifier = h_uint32();
    HParser *message_digest = h_repeat_n(h_uint8(), 16);

    HParser *fixed_fields = h_sequence(
        li_vn_mode, stratum, poll, precision,
        root_delay, root_dispersion, reference_id,
        timestamp, timestamp, timestamp, timestamp,
        NULL);

    HParser *auth_fields = h_sequence(
        key_identifier, message_digest, NULL);
    
    return h_sequence(
        fixed_fields,
        h_optional(auth_fields),
        NULL);
}

void print_ntp_packet(ntp_packet *pkt) {
    printf("LI-VN-Mode: 0x%x\n", pkt->li_vn_mode);
    printf("Stratum: %u\n", pkt->stratum);
    printf("Poll: %u\n", pkt->poll);
    printf("Precision: %d\n", pkt->precision);
    printf("Root Delay: %u\n", pkt->root_delay);
    printf("Root Dispersion: %u\n", pkt->root_dispersion);
    printf("Reference ID: %u\n", pkt->reference_id);
    printf("Reference Timestamp: %llu\n", pkt->reference_timestamp);
    printf("Originate Timestamp: %llu\n", pkt->originate_timestamp);
    printf("Receive Timestamp: %llu\n", pkt->receive_timestamp);
    printf("Transmit Timestamp: %llu\n", pkt->transmit_timestamp);
    if (pkt->has_auth) {
        printf("Key Identifier: %u\n", pkt->key_identifier);
        printf("Message Digest: ");
        for (int i = 0; i < 16; i++) {
            printf("%02x", pkt->message_digest[i]);
        }
        printf("\n");
    }
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Error allocating memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    ntp_packet pkt;
    memset(&pkt, 0, sizeof(ntp_packet));

    HParser *parser = ntp_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result->ast) {
        // Map the parsed data to the ntp_packet structure
        const HParsedToken *ast = result->ast;
        const uint8_t *data = H_CAST_UINT8(ast);

        pkt.li_vn_mode = data[0];
        pkt.stratum = data[1];
        pkt.poll = data[2];
        pkt.precision = (int8_t)data[3];
        pkt.root_delay = *(uint32_t *)(data + 4);
        pkt.root_dispersion = *(uint32_t *)(data + 8);
        pkt.reference_id = *(uint32_t *)(data + 12);
        pkt.reference_timestamp = *(uint64_t *)(data + 16);
        pkt.originate_timestamp = *(uint64_t *)(data + 24);
        pkt.receive_timestamp = *(uint64_t *)(data + 32);
        pkt.transmit_timestamp = *(uint64_t *)(data + 40);
        
        if (ast->length > 48) { // Check if authentication fields are present
            pkt.has_auth = 1;
            pkt.key_identifier = *(uint32_t *)(data + 48);
            memcpy(pkt.message_digest, data + 52, 16);
        }

        print_ntp_packet(&pkt);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Error parsing NTP packet\n");
        h_parse_result_free(result);
    }

    free(buffer);
    return EXIT_SUCCESS;
}