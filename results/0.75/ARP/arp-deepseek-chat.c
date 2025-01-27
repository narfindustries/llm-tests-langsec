#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

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

static HParser *arp_parser() {
    return h_sequence(
        h_bits(16, NULL), // htype
        h_bits(16, NULL), // ptype
        h_bits(8, NULL),  // hlen
        h_bits(8, NULL),  // plen
        h_bits(16, NULL), // oper
        h_bits(48, NULL), // sha
        h_bits(32, NULL), // spa
        h_bits(48, NULL), // tha
        h_bits(32, NULL), // tpa
        NULL
    );
}

static void parse_arp(const uint8_t *data, size_t length) {
    HParseResult *result = h_parse(arp_parser(), data, length);
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
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(length);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, length, file);
    fclose(file);

    parse_arp(data, length);

    free(data);
    return 0;
}