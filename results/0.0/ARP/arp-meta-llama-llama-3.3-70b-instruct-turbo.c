#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define ETHERNET_HW_TYPE 1
#define IPV4_PROTO_TYPE 0x0800
#define IPV4_ADDR_LEN 4
#define ETHERNET_HW_ADDR_LEN 6

typedef struct {
    uint16_t htype;
    uint16_t ptype;
    uint8_t hlen;
    uint8_t plen;
    uint16_t oper;
    uint8_t sha[ETHERNET_HW_ADDR_LEN];
    uint32_t spa;
    uint8_t tha[ETHERNET_HW_ADDR_LEN];
    uint32_t tpa;
} arp_packet_t;

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        printf("Failed to open file %s\n", argv[1]);
        return 1;
    }

    size_t size = 28;
    uint8_t *data = malloc(size);
    if (fread(data, 1, size, file) != size) {
        printf("Failed to read file %s\n", argv[1]);
        return 1;
    }

    arp_packet_t *packet = (arp_packet_t *)data;

    printf("Hardware Type: %u\n", packet->htype);
    printf("Protocol Type: 0x%04x\n", packet->ptype);
    printf("Hardware Address Length: %u\n", packet->hlen);
    printf("Protocol Address Length: %u\n", packet->plen);
    printf("Operation: %u\n", packet->oper);
    printf("Sender Hardware Address: ");
    for (int i = 0; i < ETHERNET_HW_ADDR_LEN; i++) {
        printf("%02x", packet->sha[i]);
    }
    printf("\n");
    printf("Sender Protocol Address: 0x%08x\n", packet->spa);
    printf("Target Hardware Address: ");
    for (int i = 0; i < ETHERNET_HW_ADDR_LEN; i++) {
        printf("%02x", packet->tha[i]);
    }
    printf("\n");
    printf("Target Protocol Address: 0x%08x\n", packet->tpa);

    free(data);
    fclose(file);
    return 0;
}