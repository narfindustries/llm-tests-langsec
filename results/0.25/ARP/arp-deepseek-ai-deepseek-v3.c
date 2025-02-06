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
    uint8_t spa[4];
    uint8_t tha[6];
    uint8_t tpa[4];
} arp_packet_t;

HParser *arp_parser() {
    return h_sequence(
        h_bits(16, NULL),  // HTYPE
        h_bits(16, NULL),  // PTYPE
        h_bits(8, NULL),   // HLEN
        h_bits(8, NULL),   // PLEN
        h_bits(16, NULL),  // OPER
        h_bits(8 * 6, NULL),  // SHA
        h_bits(8 * 4, NULL),  // SPA
        h_bits(8 * 6, NULL),  // THA
        h_bits(8 * 4, NULL),  // TPA
        NULL
    );
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

    HParser *parser = arp_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (!result) {
        fprintf(stderr, "Failed to parse ARP packet\n");
        free(buffer);
        return 1;
    }

    uint8_t *data = (uint8_t *)result->ast;
    arp_packet_t arp;
    arp.htype = (data[0] << 8) | data[1];
    arp.ptype = (data[2] << 8) | data[3];
    arp.hlen = data[4];
    arp.plen = data[5];
    arp.oper = (data[6] << 8) | data[7];
    for (int i = 0; i < 6; i++) arp.sha[i] = data[8 + i];
    for (int i = 0; i < 4; i++) arp.spa[i] = data[14 + i];
    for (int i = 0; i < 6; i++) arp.tha[i] = data[18 + i];
    for (int i = 0; i < 4; i++) arp.tpa[i] = data[24 + i];

    printf("HTYPE: %04x\n", arp.htype);
    printf("PTYPE: %04x\n", arp.ptype);
    printf("HLEN: %02x\n", arp.hlen);
    printf("PLEN: %02x\n", arp.plen);
    printf("OPER: %04x\n", arp.oper);
    printf("SHA: %02x:%02x:%02x:%02x:%02x:%02x\n", arp.sha[0], arp.sha[1], arp.sha[2], arp.sha[3], arp.sha[4], arp.sha[5]);
    printf("SPA: %d.%d.%d.%d\n", arp.spa[0], arp.spa[1], arp.spa[2], arp.spa[3]);
    printf("THA: %02x:%02x:%02x:%02x:%02x:%02x\n", arp.tha[0], arp.tha[1], arp.tha[2], arp.tha[3], arp.tha[4], arp.tha[5]);
    printf("TPA: %d.%d.%d.%d\n", arp.tpa[0], arp.tpa[1], arp.tpa[2], arp.tpa[3]);

    h_parse_result_free(result);
    free(buffer);

    return 0;
}