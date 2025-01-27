#include <stdio.h>
#include <stdint.h>
#include <string.h>

// Define the structure for the Network Time Protocol Version 4 (NTPv4) packet
typedef struct {
    uint8_t leap;          // Leap indicator (2 bits) and version (3 bits) and mode (3 bits)
    uint8_t poll;          // Poll interval (eight bits, signed)
    uint8_t precision;     // Precision of the local clock (eight bits, signed)
    uint32_t delay;        // Round-trip delay (32 bits, unsigned)
    uint32_t dispersion;   // Dispersion (32 bits, unsigned)
    uint32_t identifier;   // Reference clock identifier (32 bits, unsigned)
    uint8_t reference_timestamp[8];  // Reference timestamp (64 bits, unsigned)
    uint8_t originate_timestamp[8]; // Originate timestamp (64 bits, unsigned)
    uint8_t receive_timestamp[8];   // Receive timestamp (64 bits, unsigned)
    uint8_t transmit_timestamp[8];  // Transmit timestamp (64 bits, unsigned)
} ntpv4_packet;

// Define a function to unpack an NTPv4 packet from a byte array
void unpack_ntpv4_packet(uint8_t* data, size_t length, ntpv4_packet* pkt) {
    // Check if the length is sufficient
    if (length < sizeof(ntpv4_packet)) {
        printf("Error: Insufficient data to unpack NTPv4 packet\n");
        return;
    }

    // Unpack the leap, version, and mode
    pkt->leap = (data[0] >> 6) & 0x3;
    uint8_t version = (data[0] >> 3) & 0x7;
    uint8_t mode = data[0] & 0x7;

    // Unpack the poll interval
    pkt->poll = data[1];

    // Unpack the precision
    pkt->precision = data[2];

    // Unpack the delay, dispersion, and identifier
    pkt->delay = (uint32_t)data[3] << 24 | (uint32_t)data[4] << 16 | (uint32_t)data[5] << 8 | (uint32_t)data[6];
    pkt->dispersion = (uint32_t)data[7] << 24 | (uint32_t)data[8] << 16 | (uint32_t)data[9] << 8 | (uint32_t)data[10];
    pkt->identifier = (uint32_t)data[11] << 24 | (uint32_t)data[12] << 16 | (uint32_t)data[13] << 8 | (uint32_t)data[14];

    // Unpack the reference, originate, receive, and transmit timestamps
    memcpy(pkt->reference_timestamp, &data[15], 8);
    memcpy(pkt->originate_timestamp, &data[23], 8);
    memcpy(pkt->receive_timestamp, &data[31], 8);
    memcpy(pkt->transmit_timestamp, &data[39], 8);
}

int main() {
    // Example usage
    uint8_t data[] = {
        0x24, // leap, version, and mode
        0x06, // poll interval
        0xfa, // precision
        0x00, 0x00, 0x00, 0x00, // delay
        0x00, 0x00, 0x00, 0x00, // dispersion
        0x00, 0x00, 0x00, 0x00, // identifier
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // reference timestamp
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // originate timestamp
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // receive timestamp
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00  // transmit timestamp
    };

    ntpv4_packet pkt;
    unpack_ntpv4_packet(data, sizeof(data), &pkt);

    // Print the unpacked NTPv4 packet fields
    printf("Leap: %u\n", pkt.leap);
    printf("Poll interval: %u\n", pkt.poll);
    printf("Precision: %u\n", pkt.precision);
    printf("Delay: %u\n", pkt.delay);
    printf("Dispersion: %u\n", pkt.dispersion);
    printf("Identifier: %u\n", pkt.identifier);
    printf("Reference timestamp: ");
    for (int i = 0; i < 8; i++) {
        printf("%02x", pkt.reference_timestamp[i]);
    }
    printf("\n");
    printf("Originate timestamp: ");
    for (int i = 0; i < 8; i++) {
        printf("%02x", pkt.originate_timestamp[i]);
    }
    printf("\n");
    printf("Receive timestamp: ");
    for (int i = 0; i < 8; i++) {
        printf("%02x", pkt.receive_timestamp[i]);
    }
    printf("\n");
    printf("Transmit timestamp: ");
    for (int i = 0; i < 8; i++) {
        printf("%02x", pkt.transmit_timestamp[i]);
    }
    printf("\n");

    return 0;
}