#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    uint16_t htype;
    uint16_t ptype;
    uint8_t hlen;
    uint8_t plen;
    uint16_t oper;
    uint8_t sha[6];
    uint32_t spa;
    uint8_t tha[6];
    uint32_t tpa;
} ARPPacket;

HParser *arp_parser() {
    HParser *htype = h_int16();
    HParser *ptype = h_int16();
    HParser *hlen = h_uint8();
    HParser *plen = h_uint8();
    HParser *oper = h_int16();
    HParser *sha = h_bits(6 * 8, false);
    HParser *spa = h_int32();
    HParser *tha = h_bits(6 * 8, false);
    HParser *tpa = h_int32();

    return h_sequence(htype, ptype, hlen, plen, oper, sha, spa, tha, tpa, NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(arp_parser(), buffer, file_size);
    if (!result) {
        fprintf(stderr, "Failed to parse ARP packet\n");
        free(buffer);
        return 1;
    }

    ARPPacket *arp = (ARPPacket *)result->ast;
    printf("HTYPE: %d\n", arp->htype);
    printf("PTYPE: 0x%04x\n", arp->ptype);
    printf("HLEN: %d\n", arp->hlen);
    printf("PLEN: %d\n", arp->plen);
    printf("OPER: %d\n", arp->oper);
    printf("SHA: %02x:%02x:%02x:%02x:%02x:%02x\n", arp->sha[0], arp->sha[1], arp->sha[2], arp->sha[3], arp->sha[4], arp->sha[5]);
    printf("SPA: %u.%u.%u.%u\n", (arp->spa >> 24) & 0xff, (arp->spa >> 16) & 0xff, (arp->spa >> 8) & 0xff, arp->spa & 0xff);
    printf("THA: %02x:%02x:%02x:%02x:%02x:%02x\n", arp->tha[0], arp->tha[1], arp->tha[2], arp->tha[3], arp->tha[4], arp->tha[5]);
    printf("TPA: %u.%u.%u.%u\n", (arp->tpa >> 24) & 0xff, (arp->tpa >> 16) & 0xff, (arp->tpa >> 8) & 0xff, arp->tpa & 0xff);

    free(buffer);
    h_parse_result_free(result);
    return 0;
}