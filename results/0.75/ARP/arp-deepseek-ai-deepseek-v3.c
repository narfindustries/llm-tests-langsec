#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
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

HParser *uint16_parser;
HParser *uint8_parser;
HParser *arp_parser;

arp_packet_t parse_arp_packet(const uint8_t *data, size_t size) {
    HParseResult *result = h_parse(arp_parser, data, size);
    if (result == NULL) {
        fprintf(stderr, "Failed to parse ARP packet\n");
        exit(1);
    }

    HCountedArray *fields = result->ast->seq->elements;
    arp_packet_t packet = {
        .htype = *(uint16_t *)fields->elements[0]->uint,
        .ptype = *(uint16_t *)fields->elements[1]->uint,
        .hlen = *(uint8_t *)fields->elements[2]->uint,
        .plen = *(uint8_t *)fields->elements[3]->uint,
        .oper = *(uint16_t *)fields->elements[4]->uint,
    };
    memcpy(packet.sha, fields->elements[5]->bytes.str, 6);
    memcpy(packet.spa, fields->elements[6]->bytes.str, 4);
    memcpy(packet.tha, fields->elements[7]->bytes.str, 6);
    memcpy(packet.tpa, fields->elements[8]->bytes.str, 4);

    h_parse_result_free(result);
    return packet;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (file == NULL) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(size);
    if (data == NULL) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, size, file);
    fclose(file);

    uint16_parser = h_uint16();
    uint8_parser = h_uint8();
    arp_parser = h_sequence(
        uint16_parser, // htype
        uint16_parser, // ptype
        uint8_parser,  // hlen
        uint8_parser,  // plen
        uint16_parser, // oper
        h_bits(6 * 8, false), // sha
        h_bits(4 * 8, false), // spa
        h_bits(6 * 8, false), // tha
        h_bits(4 * 8, false), // tpa
        NULL
    );

    arp_packet_t packet = parse_arp_packet(data, size);
    free(data);

    printf("ARP Packet:\n");
    printf("  HTYPE: %04x\n", packet.htype);
    printf("  PTYPE: %04x\n", packet.ptype);
    printf("  HLEN: %02x\n", packet.hlen);
    printf("  PLEN: %02x\n", packet.plen);
    printf("  OPER: %04x\n", packet.oper);
    printf("  SHA: %02x:%02x:%02x:%02x:%02x:%02x\n", packet.sha[0], packet.sha[1], packet.sha[2], packet.sha[3], packet.sha[4], packet.sha[5]);
    printf("  SPA: %d.%d.%d.%d\n", packet.spa[0], packet.spa[1], packet.spa[2], packet.spa[3]);
    printf("  THA: %02x:%02x:%02x:%02x:%02x:%02x\n", packet.tha[0], packet.tha[1], packet.tha[2], packet.tha[3], packet.tha[4], packet.tha[5]);
    printf("  TPA: %d.%d.%d.%d\n", packet.tpa[0], packet.tpa[1], packet.tpa[2], packet.tpa[3]);

    return 0;
}