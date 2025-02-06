#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <arpa/inet.h>

typedef struct {
    uint16_t htype;
    uint16_t ptype;
    uint8_t hlen;
    uint8_t plen;
    uint16_t opcode;
    uint8_t sha[6];
    uint8_t spa[4];
    uint8_t tha[6];
    uint8_t tpa[4];
} arp_packet;

hammer_parser arp_parser() {
    return h_seq(
        h_uint16_be(),
        h_uint16_be(),
        h_uint8(),
        h_uint8(),
        h_uint16_be(),
        h_array(h_uint8(), 6),
        h_array(h_uint8(), 4),
        h_array(h_uint8(), 6),
        h_array(h_uint8(), 4),
        h_end()
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buffer = (uint8_t *)malloc(fsize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }

    fread(buffer, 1, fsize, fp);
    fclose(fp);

    hammer_result result = hammer_parse(arp_parser(), buffer, fsize);

    if (result.success) {
        arp_packet *packet = (arp_packet *)result.value;
        char spa_str[INET_ADDRSTRLEN];
        char tpa_str[INET_ADDRSTRLEN];

        inet_ntop(AF_INET, packet->spa, spa_str, INET_ADDRSTRLEN);
        inet_ntop(AF_INET, packet->tpa, tpa_str, INET_ADDRSTRLEN);

        printf("Hardware Type: %u\n", ntohs(packet->htype));
        printf("Protocol Type: %u\n", ntohs(packet->ptype));
        printf("Hardware Address Length: %u\n", packet->hlen);
        printf("Protocol Address Length: %u\n", packet->plen);
        printf("Opcode: %u\n", ntohs(packet->opcode));
        printf("Sender Hardware Address: %02x:%02x:%02x:%02x:%02x:%02x\n", 
               packet->sha[0], packet->sha[1], packet->sha[2], packet->sha[3], packet->sha[4], packet->sha[5]);
        printf("Sender Protocol Address: %s\n", spa_str);
        printf("Target Hardware Address: %02x:%02x:%02x:%02x:%02x:%02x\n",
               packet->tha[0], packet->tha[1], packet->tha[2], packet->tha[3], packet->tha[4], packet->tha[5]);
        printf("Target Protocol Address: %s\n", tpa_str);

        free(result.value);
    } else {
        fprintf(stderr, "Parsing failed at offset %zu: %s\n", result.offset, result.error);
        free(result.value);
        return 1;
    }

    free(buffer);
    return 0;
}
