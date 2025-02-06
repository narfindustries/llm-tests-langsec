#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

typedef struct {
    uint8_t li_vn_mode;
    uint8_t stratum;
    uint8_t poll;
    int8_t precision;
    uint32_t root_delay;
    uint32_t root_dispersion;
    uint32_t ref_id;
    uint64_t ref_timestamp;
    uint64_t orig_timestamp;
    uint64_t recv_timestamp;
    uint64_t trans_timestamp;
} ntp_packet_t;

HParser *ntp_packet_parser(void) {
    HParser *li_vn_mode = h_bits(8, false);
    HParser *stratum = h_bits(8, false);
    HParser *poll = h_bits(8, false);
    HParser *precision = h_bits(8, true);
    HParser *root_delay = h_bits(32, false);
    HParser *root_dispersion = h_bits(32, false);
    HParser *ref_id = h_bits(32, false);
    HParser *ref_timestamp = h_bits(64, false);
    HParser *orig_timestamp = h_bits(64, false);
    HParser *recv_timestamp = h_bits(64, false);
    HParser *trans_timestamp = h_bits(64, false);

    return h_sequence(
        li_vn_mode,
        stratum,
        poll,
        precision,
        root_delay,
        root_dispersion,
        ref_id,
        ref_timestamp,
        orig_timestamp,
        recv_timestamp,
        trans_timestamp,
        NULL
    );
}

void print_ntp_packet(ntp_packet_t *packet) {
    printf("LI_VN_MODE: %02x\n", packet->li_vn_mode);
    printf("Stratum: %u\n", packet->stratum);
    printf("Poll: %u\n", packet->poll);
    printf("Precision: %d\n", packet->precision);
    printf("Root Delay: %u\n", packet->root_delay);
    printf("Root Dispersion: %u\n", packet->root_dispersion);
    printf("Reference ID: %u\n", packet->ref_id);
    printf("Reference Timestamp: %llu\n", packet->ref_timestamp);
    printf("Originate Timestamp: %llu\n", packet->orig_timestamp);
    printf("Receive Timestamp: %llu\n", packet->recv_timestamp);
    printf("Transmit Timestamp: %llu\n", packet->trans_timestamp);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("fopen");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("malloc");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = ntp_packet_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        ntp_packet_t packet;
        const uint8_t *data = result->ast->seq->elements[0]->bytes;

        packet.li_vn_mode = data[0];
        packet.stratum = data[1];
        packet.poll = data[2];
        packet.precision = data[3];
        packet.root_delay = (data[4] << 24) | (data[5] << 16) | (data[6] << 8) | data[7];
        packet.root_dispersion = (data[8] << 24) | (data[9] << 16) | (data[10] << 8) | data[11];
        packet.ref_id = (data[12] << 24) | (data[13] << 16) | (data[14] << 8) | data[15];
        packet.ref_timestamp = ((uint64_t)data[16] << 56) | ((uint64_t)data[17] << 48) | ((uint64_t)data[18] << 40) | ((uint64_t)data[19] << 32) |
                               ((uint64_t)data[20] << 24) | ((uint64_t)data[21] << 16) | ((uint64_t)data[22] << 8) | (uint64_t)data[23];
        packet.orig_timestamp = ((uint64_t)data[24] << 56) | ((uint64_t)data[25] << 48) | ((uint64_t)data[26] << 40) | ((uint64_t)data[27] << 32) |
                                ((uint64_t)data[28] << 24) | ((uint64_t)data[29] << 16) | ((uint64_t)data[30] << 8) | (uint64_t)data[31];
        packet.recv_timestamp = ((uint64_t)data[32] << 56) | ((uint64_t)data[33] << 48) | ((uint64_t)data[34] << 40) | ((uint64_t)data[35] << 32) |
                                ((uint64_t)data[36] << 24) | ((uint64_t)data[37] << 16) | ((uint64_t)data[38] << 8) | (uint64_t)data[39];
        packet.trans_timestamp = ((uint64_t)data[40] << 56) | ((uint64_t)data[41] << 48) | ((uint64_t)data[42] << 40) | ((uint64_t)data[43] << 32) |
                                 ((uint64_t)data[44] << 24) | ((uint64_t)data[45] << 16) | ((uint64_t)data[46] << 8) | (uint64_t)data[47];

        print_ntp_packet(&packet);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse NTP packet\n");
    }

    free(buffer);
    h_parser_free(parser);

    return EXIT_SUCCESS;
}