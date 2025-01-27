#include <hammer/core.h>

typedef struct {
    uint8_t li_vn_mode;
    uint8_t stratum;
    uint8_t poll;
    uint8_t precision;
    uint32_t root_delay;
    uint32_t root_dispersion;
    uint32_t reference_id;
    uint64_t reference_timestamp;
    uint64_t originate_timestamp;
    uint64_t receive_timestamp;
    uint64_t transmit_timestamp;
} ntp_packet;

static HParsedToken *parse_ntp_packet(const HParseResult *p, void *user_data) {
    ntp_packet *packet = H_ALLOC(ntp_packet);

    packet->li_vn_mode = H_CAST_UINT(p->ast->seq->elements[0], uint8_t);
    packet->stratum = H_CAST_UINT(p->ast->seq->elements[1], uint8_t);
    packet->poll = H_CAST_UINT(p->ast->seq->elements[2], uint8_t);
    packet->precision = H_CAST_UINT(p->ast->seq->elements[3], uint8_t);
    packet->root_delay = H_CAST_UINT(p->ast->seq->elements[4], uint32_t);
    packet->root_dispersion = H_CAST_UINT(p->ast->seq->elements[5], uint32_t);
    packet->reference_id = H_CAST_UINT(p->ast->seq->elements[6], uint32_t);
    packet->reference_timestamp = H_CAST_UINT(p->ast->seq->elements[7], uint64_t);
    packet->originate_timestamp = H_CAST_UINT(p->ast->seq->elements[8], uint64_t);
    packet->receive_timestamp = H_CAST_UINT(p->ast->seq->elements[9], uint64_t);
    packet->transmit_timestamp = H_CAST_UINT(p->ast->seq->elements[10], uint64_t);

    return H_MAKE(PACKAGE_SUCCESS, packet);
}

HParser *create_ntp_parser(void) {
    HParser *leap_indicator = h_bits(2, false);
    HParser *version_number = h_bits(3, false);
    HParser *mode = h_bits(3, false);
    HParser *li_vn_mode = h_seq(h_binary(leap_indicator, version_number, mode, NULL), NULL);

    HParser *stratum = h_uint8();
    HParser *poll = h_uint8();
    HParser *precision = h_uint8();
    HParser *root_delay = h_uint32();
    HParser *root_dispersion = h_uint32();
    HParser *reference_id = h_uint32();
    HParser *reference_timestamp = h_uint64();
    HParser *originate_timestamp = h_uint64();
    HParser *receive_timestamp = h_uint64();
    HParser *transmit_timestamp = h_uint64();

    HParser *ntp_sequence = h_sequence(
        li_vn_mode, stratum, poll, precision, root_delay, root_dispersion,
        reference_id, reference_timestamp, originate_timestamp,
        receive_timestamp, transmit_timestamp, NULL);

    return h_action(ntp_sequence, parse_ntp_packet, NULL);
}

void destroy_ntp_packet(ntp_packet *packet) {
    if (packet) {
        H_FREE(packet);
    }
}