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

static HParsedToken *parse_arp_packet(const HParseResult *p, void *user_data) {
    arp_packet_t *arp = malloc(sizeof(arp_packet_t));
    if (!arp) return NULL;

    const HParsedToken *tokens = p->ast->token_sequence->elements;
    arp->htype = tokens[0]->uint16;
    arp->ptype = tokens[1]->uint16;
    arp->hlen  = tokens[2]->uint8;
    arp->plen  = tokens[3]->uint8;
    arp->oper  = tokens[4]->uint16;
    memcpy(arp->sha, tokens[5]->bytes, 6);
    memcpy(arp->spa, tokens[6]->bytes, 4);
    memcpy(arp->tha, tokens[7]->bytes, 6);
    memcpy(arp->tpa, tokens[8]->bytes, 4);

    HParsedToken *result = H_MAKE(H_OBJECT, arp);
    return result;
}

HParser *arp_parser(void) {
    HParser *htype = h_uint16();
    HParser *ptype = h_uint16();
    HParser *hlen  = h_uint8();
    HParser *plen  = h_uint8();
    HParser *oper  = h_uint16();
    HParser *sha   = h_fixed_bytes(6);
    HParser *spa   = h_fixed_bytes(4);
    HParser *tha   = h_fixed_bytes(6);
    HParser *tpa   = h_fixed_bytes(4);

    HParser *arp_packet = h_sequence(
        htype, ptype, hlen, plen, oper, sha, spa, tha, tpa, NULL
    );
    return h_action(arp_packet, parse_arp_packet, NULL);
}

void free_arp_packet(HParsedToken *token) {
    free(H_CAST(arp_packet_t, token));
}

int main(int argc, char **argv) {
    HParser *parser = arp_parser();
    HParseResult *result;

    // Example ARP packet data
    uint8_t arp_data[] = {
        0x00, 0x01, 0x08, 0x00, 0x06, 0x04, 0x00, 0x01,
        0x00, 0x0c, 0x29, 0x3e, 0x5c, 0x47, 0xc0, 0xa8,
        0x01, 0x68, 0x00, 0x0c, 0x29, 0x3e, 0x5c, 0x48,
        0xc0, 0xa8, 0x01, 0x01
    };

    result = h_parse(parser, arp_data, sizeof(arp_data));

    if (result && result->ast) {
        arp_packet_t *arp = H_CAST(arp_packet_t, result->ast);
        printf("Parsed ARP packet:\n");
        printf("HTYPE: %u\n", arp->htype);
        printf("PTYPE: %u\n", arp->ptype);
        printf("HLEN: %u\n", arp->hlen);
        printf("PLEN: %u\n", arp->plen);
        printf("OPER: %u\n", arp->oper);
        printf("SHA: %02x:%02x:%02x:%02x:%02x:%02x\n",
               arp->sha[0], arp->sha[1], arp->sha[2],
               arp->sha[3], arp->sha[4], arp->sha[5]);
        printf("SPA: %u.%u.%u.%u\n",
               arp->spa[0], arp->spa[1], arp->spa[2], arp->spa[3]);
        printf("THA: %02x:%02x:%02x:%02x:%02x:%02x\n",
               arp->tha[0], arp->tha[1], arp->tha[2],
               arp->tha[3], arp->tha[4], arp->tha[5]);
        printf("TPA: %u.%u.%u.%u\n",
               arp->tpa[0], arp->tpa[1], arp->tpa[2], arp->tpa[3]);

        free_arp_packet(result->ast);
    } else {
        printf("Failed to parse ARP packet.\n");
    }

    h_parse_result_free(result);
    h_parser_free(parser);

    return 0;
}