#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define ARP packet structure
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
} ARP_Packet;

// Function to parse ARP packet
HParser *create_arp_parser() {
    HParser *htype = h_uint16();
    HParser *ptype = h_uint16();
    HParser *hlen = h_uint8();
    HParser *plen = h_uint8();
    HParser *oper = h_uint16();
    HParser *sha = h_repeat_n(h_uint8(), 6);
    HParser *spa = h_repeat_n(h_uint8(), 4);
    HParser *tha = h_repeat_n(h_uint8(), 6);
    HParser *tpa = h_repeat_n(h_uint8(), 4);

    return h_sequence(htype, ptype, hlen, plen, oper, sha, spa, tha, tpa, NULL);
}

// Function to print ARP packet
void print_arp_packet(const ARP_Packet *packet) {
    printf("Hardware Type: %u\n", packet->htype);
    printf("Protocol Type: %u\n", packet->ptype);
    printf("Hardware Length: %u\n", packet->hlen);
    printf("Protocol Length: %u\n", packet->plen);
    printf("Operation: %u\n", packet->oper);
    printf("Sender Hardware Address: ");
    for (int i = 0; i < 6; i++) {
        printf("%02X", packet->sha[i]);
        if (i < 5) printf(":");
    }
    printf("\nSender Protocol Address: ");
    for (int i = 0; i < 4; i++) {
        printf("%d", packet->spa[i]);
        if (i < 3) printf(".");
    }
    printf("\nTarget Hardware Address: ");
    for (int i = 0; i < 6; i++) {
        printf("%02X", packet->tha[i]);
        if (i < 5) printf(":");
    }
    printf("\nTarget Protocol Address: ");
    for (int i = 0; i < 4; i++) {
        printf("%d", packet->tpa[i]);
        if (i < 3) printf(".");
    }
    printf("\n");
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

    HParser *arp_parser = create_arp_parser();
    HParseResult *result = h_parse(arp_parser, buffer, file_size);
    if (result && result->ast) {
        ARP_Packet *packet = (ARP_Packet *)result->ast->seq->elements[0]->user;
        print_arp_packet(packet);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse ARP packet\n");
    }

    free(buffer);
    h_parser_free(arp_parser);
    return 0;
}