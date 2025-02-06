#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define constants for ARP packet
#define ETHERNET_HARDWARE_TYPE 1
#define IPV4_PROTOCOL_TYPE 0x0800
#define MAC_LENGTH 6
#define IPV4_LENGTH 4

// ARP packet structure
typedef struct {
    uint16_t htype;
    uint16_t ptype;
    uint8_t hlen;
    uint8_t plen;
    uint16_t oper;
    uint8_t sha[MAC_LENGTH];
    uint8_t spa[IPV4_LENGTH];
    uint8_t tha[MAC_LENGTH];
    uint8_t tpa[IPV4_LENGTH];
} ARP_Packet;

// Hammer parsers for ARP packet fields
HParser *parse_arp() {
    HParser *htype = h_uint16();
    HParser *ptype = h_uint16();
    HParser *hlen = h_uint8();
    HParser *plen = h_uint8();
    HParser *oper = h_uint16();
    HParser *sha = h_repeat_n(h_uint8(), MAC_LENGTH);
    HParser *spa = h_repeat_n(h_uint8(), IPV4_LENGTH);
    HParser *tha = h_repeat_n(h_uint8(), MAC_LENGTH);
    HParser *tpa = h_repeat_n(h_uint8(), IPV4_LENGTH);

    return h_sequence(htype, ptype, hlen, plen, oper, sha, spa, tha, tpa, NULL);
}

// Function to read file into memory
uint8_t *read_file(const char *filename, size_t *length) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Unable to open file");
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    *length = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(*length);
    if (!data) {
        perror("Memory allocation failed");
        fclose(file);
        return NULL;
    }

    if (fread(data, 1, *length, file) != *length) {
        perror("Failed to read file");
        free(data);
        fclose(file);
        return NULL;
    }

    fclose(file);
    return data;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <ARP packet file>\n", argv[0]);
        return 1;
    }

    size_t length;
    uint8_t *data = read_file(argv[1], &length);
    if (!data) {
        return 1;
    }

    HParser *arp_parser = parse_arp();
    HParseResult *result = h_parse(arp_parser, data, length);
    if (result) {
        printf("ARP packet parsed successfully.\n");
        ARP_Packet *arp_packet = (ARP_Packet *)result->ast;
        printf("Hardware Type: %u\n", arp_packet->htype);
        printf("Protocol Type: %u\n", arp_packet->ptype);
        printf("Hardware Length: %u\n", arp_packet->hlen);
        printf("Protocol Length: %u\n", arp_packet->plen);
        printf("Operation: %u\n", arp_packet->oper);
    } else {
        fprintf(stderr, "Failed to parse ARP packet.\n");
    }

    h_parse_result_free(result);
    h_parser_free(arp_parser);
    free(data);

    return 0;
}