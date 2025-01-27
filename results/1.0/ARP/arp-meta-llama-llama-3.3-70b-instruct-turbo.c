#include <stdio.h>
#include <stdint.h>
#include <string.h>

// Define the structure for the ARP packet
typedef struct {
    uint16_t htype;
    uint16_t ptype;
    uint8_t hlen;
    uint8_t plen;
    uint16_t oper;
    uint8_t sha[6];
    uint32_t spa;
    uint8_t tha[6];
    uint32_t tpa;
} arp_packet_t;

// Define the Hammer specification
typedef struct {
    uint32_t magic;
    uint32_t version;
    uint32_t packet_type;
    uint32_t packet_length;
    arp_packet_t arp;
} hammer_packet_t;

int main() {
    // Create a sample ARP packet
    arp_packet_t arp = {
        .htype = 0x0001,
        .ptype = 0x0800,
        .hlen = 6,
        .plen = 4,
        .oper = 0x0001,
        .sha = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00},
        .spa = 0x0a000001,
        .tha = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00},
        .tpa = 0x0a000002
    };

    // Create a sample Hammer packet
    hammer_packet_t packet = {
        .magic = 0x484d5201,
        .version = 1,
        .packet_type = 1,
        .packet_length = sizeof(arp_packet_t),
        .arp = arp
    };

    // Print the Hammer packet
    printf("Magic: 0x%x\n", packet.magic);
    printf("Version: 0x%x\n", packet.version);
    printf("Packet Type: 0x%x\n", packet.packet_type);
    printf("Packet Length: 0x%x\n", packet.packet_length);
    printf("ARP Packet:\n");
    printf("  HType: 0x%x\n", packet.arp.htype);
    printf("  PType: 0x%x\n", packet.arp.ptype);
    printf("  HLen: 0x%x\n", packet.arp.hlen);
    printf("  PLen: 0x%x\n", packet.arp.plen);
    printf("  Oper: 0x%x\n", packet.arp.oper);
    printf("  SHA: ");
    for (int i = 0; i < 6; i++) {
        printf("%02x", packet.arp.sha[i]);
    }
    printf("\n");
    printf("  SPA: 0x%x\n", packet.arp.spa);
    printf("  THA: ");
    for (int i = 0; i < 6; i++) {
        printf("%02x", packet.arp.tha[i]);
    }
    printf("\n");
    printf("  TPA: 0x%x\n", packet.arp.tpa);

    return 0;
}