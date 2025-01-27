#include <hammer/hammer.h>

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
} ntp_packet_t;

static HParser *ntp_packet_parser(void) {
    HParser *li_vn_mode = h_bits(8, false);
    HParser *stratum = h_bits(8, false);
    HParser *poll = h_bits(8, false);
    HParser *precision = h_bits(8, false);
    HParser *root_delay = h_bits(32, false);
    HParser *root_dispersion = h_bits(32, false);
    HParser *reference_id = h_bits(32, false);
    HParser *reference_timestamp = h_bits(64, false);
    HParser *originate_timestamp = h_bits(64, false);
    HParser *receive_timestamp = h_bits(64, false);
    HParser *transmit_timestamp = h_bits(64, false);

    HParser *ntp_packet = h_sequence(
        li_vn_mode,
        stratum,
        poll,
        precision,
        root_delay,
        root_dispersion,
        reference_id,
        reference_timestamp,
        originate_timestamp,
        receive_timestamp,
        transmit_timestamp,
        NULL
    );

    return ntp_packet;
}

int main(int argc, char **argv) {
    HParser *parser = ntp_packet_parser();
    // Use the parser with your data here
    h_parser_free(parser);
    return 0;
}