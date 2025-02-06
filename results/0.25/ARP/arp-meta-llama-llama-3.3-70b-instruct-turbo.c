#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define ARP_ETHERNET 1
#define ARP_IPV4 0x0800

typedef struct {
    uint16_t htype;
    uint16_t ptype;
    uint8_t hlen;
    uint8_t plen;
    uint16_t oper;
} arp_header_t;

typedef struct {
    uint8_t addr[6];
} ethernet_addr_t;

typedef struct {
    uint32_t addr;
} ipv4_addr_t;

typedef struct {
    arp_header_t header;
    ethernet_addr_t sha;
    ipv4_addr_t spa;
    ethernet_addr_t tha;
    ipv4_addr_t tpa;
} arp_packet_t;

void print_arp_packet(arp_packet_t *packet) {
    printf("ARP Packet:\n");
    printf("  Header:\n");
    printf("    Hardware Type: %u\n", packet->header.htype);
    printf("    Protocol Type: 0x%04x\n", packet->header.ptype);
    printf("    Hardware Address Length: %u\n", packet->header.hlen);
    printf("    Protocol Address Length: %u\n", packet->header.plen);
    printf("    Operation: %u\n", packet->header.oper);
    printf("  Sender Hardware Address: ");
    for (int i = 0; i < 6; i++) {
        printf("%02x:", packet->sha.addr[i]);
    }
    printf("\n");
    printf("  Sender Protocol Address: 0x%08x\n", packet->spa.addr);
    printf("  Target Hardware Address: ");
    for (int i = 0; i < 6; i++) {
        printf("%02x:", packet->tha.addr[i]);
    }
    printf("\n");
    printf("  Target Protocol Address: 0x%08x\n", packet->tpa.addr);
}

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

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t *data = malloc(file_size);
    if (!data) {
        printf("Failed to allocate memory\n");
        fclose(file);
        return 1;
    }

    size_t bytes_read = fread(data, 1, file_size, file);
    if (bytes_read != file_size) {
        printf("Failed to read file %s\n", argv[1]);
        free(data);
        fclose(file);
        return 1;
    }

    fclose(file);

    arp_packet_t *packet = (arp_packet_t *)data;
    print_arp_packet(packet);

    free(data);
    return 0;
}