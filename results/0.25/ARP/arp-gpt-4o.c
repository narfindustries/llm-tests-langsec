#include <hammer/hammer.h>

typedef struct {
    uint16_t htype;
    uint16_t ptype;
    uint8_t hlen;
    uint8_t plen;
    uint16_t oper;
    uint8_t sha[6];
    uint8_t spa[4];
    uint8_t tha[6];
    uint8_t tpa[4];
} arp_packet_t;

static HParser *create_arp_parser(void) {
    HParser *htype = h_uint16();
    HParser *ptype = h_uint16();
    HParser *hlen = h_uint8();
    HParser *plen = h_uint8();
    HParser *oper = h_uint16();
    HParser *sha = h_repeat_n(h_uint8(), 6);
    HParser *spa = h_repeat_n(h_uint8(), 4);
    HParser *tha = h_repeat_n(h_uint8(), 6);
    HParser *tpa = h_repeat_n(h_uint8(), 4);

    HParser *arp_packet = h_sequence(htype, ptype, hlen, plen, oper, sha, spa, tha, tpa, NULL);

    return arp_packet;
}

int main(int argc, char **argv) {
    HParser *arp_parser = create_arp_parser();
    // Add code to use the parser, e.g., parse a buffer
    h_parser_free(arp_parser);
    return 0;
}