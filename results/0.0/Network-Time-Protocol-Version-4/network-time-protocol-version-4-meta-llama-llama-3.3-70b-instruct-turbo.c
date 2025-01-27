#include <stdio.h>
#include <stdint.h>
#include <string.h>

// Define the structure for Network Time Protocol Version 4
typedef struct {
    uint8_t leap_indicator : 2;
    uint8_t version : 3;
    uint8_t mode : 3;
    uint8_t stratum;
    uint8_t poll;
    uint8_t precision;
    uint32_t root_delay;
    uint32_t root_dispersion;
    uint32_t reference_id;
    uint64_t reference_timestamp;
    uint64_t originate_timestamp;
    uint64_t receive_timestamp;
    uint64_t transmit_timestamp;
} ntp_packet_t;

// Function to parse NTP packet
void parse_ntp_packet(uint8_t* packet, size_t length) {
    ntp_packet_t* ntp_packet = (ntp_packet_t*) packet;

    // Check if the packet is at least the minimum size
    if (length < sizeof(ntp_packet_t)) {
        printf("Invalid packet length\n");
        return;
    }

    // Print the parsed fields
    printf("Leap Indicator: %u\n", ntp_packet->leap_indicator);
    printf("Version: %u\n", ntp_packet->version);
    printf("Mode: %u\n", ntp_packet->mode);
    printf("Stratum: %u\n", ntp_packet->stratum);
    printf("Poll: %u\n", ntp_packet->poll);
    printf("Precision: %u\n", ntp_packet->precision);
    printf("Root Delay: %u\n", ntp_packet->root_delay);
    printf("Root Dispersion: %u\n", ntp_packet->root_dispersion);
    printf("Reference ID: %u\n", ntp_packet->reference_id);
    printf("Reference Timestamp: %llu\n", ntp_packet->reference_timestamp);
    printf("Originate Timestamp: %llu\n", ntp_packet->originate_timestamp);
    printf("Receive Timestamp: %llu\n", ntp_packet->receive_timestamp);
    printf("Transmit Timestamp: %llu\n", ntp_packet->transmit_timestamp);
}

int main() {
    // Example NTP packet
    uint8_t packet[] = {
        0x28, 0x01, 0x03, 0x02, 0x04, 0x05, 0x06, 0x07,
        0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
        0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
        0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
        0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27
    };

    parse_ntp_packet(packet, sizeof(packet));

    return 0;
}