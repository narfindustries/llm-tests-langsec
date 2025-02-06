#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

// Define hardware types
typedef enum {
    ETHERNET = 1,
    EXPERIMENTAL_ETHERNET = 2,
    AMATEUR_RADIO_AX25 = 3,
    PROTEON_PRONET_TOKEN_RING = 4,
    CHAOS = 5,
    IEEE_802_NETWORKS = 6,
    ARCNET = 7,
    HYPERCHANNEL = 8,
    LANSTAR = 9,
    AUTONET_SHORT_ADDRESS = 10,
    LOCALTALK = 11,
    LOCALNET = 12,
    ULTRALINK = 13,
    SMDS = 14,
    FRAME_RELAY = 15,
    ATM = 16,
    HDLC = 17,
    FIBRE_CHANNEL = 18,
    SERIAL_LINE = 20
} hardware_type_t;

// Define protocol types
typedef enum {
    IPv4 = 0x0800,
    ARP = 0x0806,
    WAKE_ON_LAN = 0x0842,
    TRILL = 0x22F3,
    CHLOROPLAST = 0x0804,
    NOVELL_NETWARE = 0x0835,
    IPv6 = 0x8863,
    IPv6_WITH_EXTENSIONS = 0x8864
} protocol_type_t;

// Define operations
typedef enum {
    REQUEST = 1,
    REPLY = 2,
    REQUEST_REVERSE = 3,
    REPLY_REVERSE = 4
} operation_t;

// Define ARP packet structure
typedef struct {
    uint16_t htype;
    uint16_t ptype;
    uint8_t hlen;
    uint8_t plen;
    uint16_t op;
    uint8_t sha[6];
    uint32_t spa;
    uint8_t tha[6];
    uint32_t tpa;
} __attribute__((packed)) arp_packet_t;

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file: %s\n", strerror(errno));
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        printf("Error allocating memory\n");
        fclose(file);
        return 1;
    }

    size_t bytes_read = fread(buffer, 1, file_size, file);
    if (bytes_read != file_size) {
        printf("Error reading file: %s\n", strerror(errno));
        free(buffer);
        fclose(file);
        return 1;
    }

    fclose(file);

    arp_packet_t* packet = (arp_packet_t*) buffer;

    printf("Hardware Type: %u\n", packet->htype);
    printf("Protocol Type: 0x%04x\n", packet->ptype);
    printf("Hardware Address Length: %u\n", packet->hlen);
    printf("Protocol Address Length: %u\n", packet->plen);
    printf("Operation: %u\n", packet->op);

    printf("Sender Hardware Address: ");
    for (int i = 0; i < 6; i++) {
        printf("%02x:", packet->sha[i]);
    }
    printf("\n");

    printf("Sender Protocol Address: 0x%08x\n", packet->spa);

    printf("Target Hardware Address: ");
    for (int i = 0; i < 6; i++) {
        printf("%02x:", packet->tha[i]);
    }
    printf("\n");

    printf("Target Protocol Address: 0x%08x\n", packet->tpa);

    free(buffer);
    return 0;
}