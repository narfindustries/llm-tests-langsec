#include <hammer/hammer.h>
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

HParser *arp_parser() {
    return h_sequence(
        h_bits(16, NULL),  // HTYPE
        h_bits(16, NULL),  // PTYPE
        h_bits(8, NULL),   // HLEN
        h_bits(8, NULL),   // PLEN
        h_bits(16, NULL),  // OPER
        h_bits(8 * 6, NULL), // SHA
        h_bits(8 * 4, NULL), // SPA
        h_bits(8 * 6, NULL), // THA
        h_bits(8 * 4, NULL), // TPA
        NULL
    );
}

void print_arp_packet(const uint8_t *buffer) {
    arp_packet_t packet;
    memcpy(&packet.htype, buffer, 2);
    memcpy(&packet.ptype, buffer + 2, 2);
    packet.hlen = buffer[4];
    packet.plen = buffer[5];
    memcpy(&packet.oper, buffer + 6, 2);
    memcpy(packet.sha, buffer + 8, 6);
    memcpy(packet.spa, buffer + 14, 4);
    memcpy(packet.tha, buffer + 18, 6);
    memcpy(packet.tpa, buffer + 24, 4);

    printf("HTYPE: %04x\n", packet.htype);
    printf("PTYPE: %04x\n", packet.ptype);
    printf("HLEN: %02x\n", packet.hlen);
    printf("PLEN: %02x\n", packet.plen);
    printf("OPER: %04x\n", packet.oper);
    printf("SHA: %02x:%02x:%02x:%02x:%02x:%02x\n", 
           packet.sha[0], packet.sha[1], packet.sha[2], 
           packet.sha[3], packet.sha[4], packet.sha[5]);
    printf("SPA: %d.%d.%d.%d\n", 
           packet.spa[0], packet.spa[1], packet.spa[2], packet.spa[3]);
    printf("THA: %02x:%02x:%02x:%02x:%02x:%02x\n", 
           packet.tha[0], packet.tha[1], packet.tha[2], 
           packet.tha[3], packet.tha[4], packet.tha[5]);
    printf("TPA: %d.%d.%d.%d\n", 
           packet.tpa[0], packet.tpa[1], packet.tpa[2], packet.tpa[3]);
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

    if (result) {
        print_arp_packet(buffer);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse ARP packet\n");
    }

    free(buffer);
    return 0;
}