#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// ARP Packet Structure
typedef struct {
    uint16_t htype;
    uint16_t ptype;
    uint8_t hlen;
    uint8_t plen;
    uint16_t oper;
    uint8_t *sha;
    uint8_t *spa;
    uint8_t *tha;
    uint8_t *tpa;
} ARP_Packet;

// Parser declarations
HParser *arp_parser;

// Initialize the ARP parser
void init_arp_parser() {
    HParser *htype = h_uint16();
    HParser *ptype = h_uint16();
    HParser *hlen = h_uint8();
    HParser *plen = h_uint8();
    HParser *oper = h_uint16();
    HParser *sha = h_bits(48, false);
    HParser *spa = h_bits(32, false);
    HParser *tha = h_bits(48, false);
    HParser *tpa = h_bits(32, false);

    arp_parser = h_sequence(htype, ptype, hlen, plen, oper, sha, spa, tha, tpa, NULL);
}

// Parse ARP packet
ARP_Packet *parse_arp(const uint8_t *data, size_t length) {
    HParseResult *result = h_parse(arp_parser, data, length);
    if (!result) {
        fprintf(stderr, "Failed to parse ARP packet\n");
        return NULL;
    }

    ARP_Packet *arp_pkt = malloc(sizeof(ARP_Packet));
    if (!arp_pkt) {
        fprintf(stderr, "Failed to allocate ARP packet\n");
        return NULL;
    }

    arp_pkt->htype = *(uint16_t*)h_value_uint(result->ast->children[0]);
    arp_pkt->ptype = *(uint16_t*)h_value_uint(result->ast->children[1]);
    arp_pkt->hlen = *(uint8_t*)h_value_uint(result->ast->children[2]);
    arp_pkt->plen = *(uint8_t*)h_value_uint(result->ast->children[3]);
    arp_pkt->oper = *(uint16_t*)h_value_uint(result->ast->children[4]);
    arp_pkt->sha = h_bytes(result->ast->children[5]->token->bits, 6);
    arp_pkt->spa = h_bytes(result->ast->children[6]->token->bits, 4);
    arp_pkt->tha = h_bytes(result->ast->children[7]->token->bits, 6);
    arp_pkt->tpa = h_bytes(result->ast->children[8]->token->bits, 4);

    h_parse_result_free(result);
    return arp_pkt;
}

// Main function
int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <ARP packet file>\n", argv[0]);
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
        fprintf(stderr, "Failed to allocate buffer\n");
        fclose(file);
        return 1;
    }

    size_t read_bytes = fread(buffer, 1, file_size, file);
    if (read_bytes != file_size) {
        fprintf(stderr, "Failed to read file\n");
        free(buffer);
        fclose(file);
        return 1;
    }

    fclose(file);

    init_arp_parser();
    ARP_Packet *arp_pkt = parse_arp(buffer, file_size);
    if (arp_pkt) {
        printf("ARP Packet Parsed:\n");
        printf("Hardware Type: %u\n", arp_pkt->htype);
        printf("Protocol Type: %u\n", arp_pkt->ptype);
        printf("Hardware Address Length: %u\n", arp_pkt->hlen);
        printf("Protocol Address Length: %u\n", arp_pkt->plen);
        printf("Operation: %u\n", arp_pkt->oper);
        for (int i = 0; i < arp_pkt->hlen; i++) {
            printf("SHA[%d]: %02x\n", i, arp_pkt->sha[i]);
        }
        for (int i = 0; i < arp_pkt->plen; i++) {
            printf("SPA[%d]: %02x\n", i, arp_pkt->spa[i]);
        }
        for (int i = 0; i < arp_pkt->hlen; i++) {
            printf("THA[%d]: %02x\n", i, arp_pkt->tha[i]);
        }
        for (int i = 0; i < arp_pkt->plen; i++) {
            printf("TPA[%d]: %02x\n", i, arp_pkt->tpa[i]);
        }
        free(arp_pkt);
    }

    free(buffer);
    return 0;
}