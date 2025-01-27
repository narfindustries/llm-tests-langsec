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
} arp_packet;

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

    HParser *arp = h_sequence(
        htype,
        ptype,
        hlen,
        plen,
        oper,
        sha,
        spa,
        tha,
        tpa,
        NULL
    );

    return arp;
}

int main(int argc, char **argv) {
    HParser *arp_parser = create_arp_parser();
    if (!arp_parser) {
        fprintf(stderr, "Failed to create ARP parser\n");
        return 1;
    }

    // Example ARP packet (Ethernet and IPv4)
    uint8_t example_packet[] = {
        0x00, 0x01, 0x08, 0x00, 0x06, 0x04, 0x00, 0x01,
        0x00, 0x0c, 0x29, 0xab, 0xcd, 0xef, 0xc0, 0xa8,
        0x01, 0x01, 0x00, 0x0c, 0x29, 0x12, 0x34, 0x56,
        0xc0, 0xa8, 0x01, 0x02
    };

    HParseResult *result = h_parse(arp_parser, example_packet, sizeof(example_packet));
    if (!result) {
        fprintf(stderr, "Failed to parse ARP packet\n");
        h_delete(arp_parser);
        return 1;
    }

    arp_packet *arp_data = (arp_packet *)result->ast->data;
    printf("Parsed ARP Packet:\n");
    printf("Hardware Type: %u\n", arp_data->htype);
    printf("Protocol Type: %u\n", arp_data->ptype);
    printf("Hardware Length: %u\n", arp_data->hlen);
    printf("Protocol Length: %u\n", arp_data->plen);
    printf("Operation: %u\n", arp_data->oper);

    h_parse_result_free(result);
    h_delete(arp_parser);

    return 0;
}