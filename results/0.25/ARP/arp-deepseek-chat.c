#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <arpa/inet.h>

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

HParser *arp_parser() {
    HParser *htype = h_int16();
    HParser *ptype = h_int16();
    HParser *hlen = h_uint8();
    HParser *plen = h_uint8();
    HParser *oper = h_int16();
    HParser *sha = h_sequence(h_uint8(), h_uint8(), h_uint8(), h_uint8(), h_uint8(), h_uint8(), NULL);
    HParser *spa = h_sequence(h_uint8(), h_uint8(), h_uint8(), h_uint8(), NULL);
    HParser *tha = h_sequence(h_uint8(), h_uint8(), h_uint8(), h_uint8(), h_uint8(), h_uint8(), NULL);
    HParser *tpa = h_sequence(h_uint8(), h_uint8(), h_uint8(), h_uint8(), NULL);

    return h_sequence(htype, ptype, hlen, plen, oper, sha, spa, tha, tpa, NULL);
}

int main() {
    HParser *parser = arp_parser();
    uint8_t input[] = {
        0x00, 0x01, 0x08, 0x00, 0x06, 0x04, 0x00, 0x01,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    };
    size_t input_len = sizeof(input);

    HParseResult *result = h_parse(parser, input, input_len);
    if (result) {
        arp_packet_t *arp = (arp_packet_t *)result->ast;
        printf("ARP Packet:\n");
        printf("  HTYPE: %04x\n", arp->htype);
        printf("  PTYPE: %04x\n", arp->ptype);
        printf("  HLEN: %02x\n", arp->hlen);
        printf("  PLEN: %02x\n", arp->plen);
        printf("  OPER: %04x\n", arp->oper);
        printf("  SHA: %02x:%02x:%02x:%02x:%02x:%02x\n", arp->sha[0], arp->sha[1], arp->sha[2], arp->sha[3], arp->sha[4], arp->sha[5]);
        printf("  SPA: %d.%d.%d.%d\n", arp->spa[0], arp->spa[1], arp->spa[2], arp->spa[3]);
        printf("  THA: %02x:%02x:%02x:%02x:%02x:%02x\n", arp->tha[0], arp->tha[1], arp->tha[2], arp->tha[3], arp->tha[4], arp->tha[5]);
        printf("  TPA: %d.%d.%d.%d\n", arp->tpa[0], arp->tpa[1], arp->tpa[2], arp->tpa[3]);
        h_parse_result_free(result);
    } else {
        printf("Failed to parse ARP packet.\n");
    }

    return 0;
}