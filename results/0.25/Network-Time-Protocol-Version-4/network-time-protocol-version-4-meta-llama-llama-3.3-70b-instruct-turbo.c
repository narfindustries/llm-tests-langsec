#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

// Define the structure for the Network Time Protocol (NTP) version 4
typedef struct {
    uint8_t leap_indicator : 2;
    uint8_t version : 3;
    uint8_t mode : 3;
    uint8_t poll : 4;
    uint8_t precision : 4;
    uint32_t delay;
    uint32_t dispersion;
    uint32_t identifier;
    uint8_t reference_timestamp[8];
    uint8_t originate_timestamp[8];
    uint8_t receive_timestamp[8];
    uint8_t transmit_timestamp[8];
} ntp_packet_t;

// Define a function to parse the NTP packet
void parse_ntp_packet(uint8_t* data, size_t length) {
    if (length < sizeof(ntp_packet_t)) {
        printf("Invalid NTP packet length\n");
        return;
    }

    ntp_packet_t* packet = (ntp_packet_t*) data;

    // Check the leap indicator
    if (packet->leap_indicator > 3) {
        printf("Invalid leap indicator\n");
        return;
    }

    // Check the version
    if (packet->version != 4) {
        printf("Unsupported NTP version\n");
        return;
    }

    // Check the mode
    if (packet->mode > 7) {
        printf("Invalid mode\n");
        return;
    }

    // Print the parsed packet information
    printf("Leap indicator: %u\n", packet->leap_indicator);
    printf("Version: %u\n", packet->version);
    printf("Mode: %u\n", packet->mode);
    printf("Poll: %u\n", packet->poll);
    printf("Precision: %u\n", packet->precision);
    printf("Delay: %u\n", packet->delay);
    printf("Dispersion: %u\n", packet->dispersion);
    printf("Identifier: %u\n", packet->identifier);

    // Print the reference timestamp
    printf("Reference timestamp: ");
    for (int i = 0; i < 8; i++) {
        printf("%02x", packet->reference_timestamp[i]);
    }
    printf("\n");

    // Print the originate timestamp
    printf("Originate timestamp: ");
    for (int i = 0; i < 8; i++) {
        printf("%02x", packet->originate_timestamp[i]);
    }
    printf("\n");

    // Print the receive timestamp
    printf("Receive timestamp: ");
    for (int i = 0; i < 8; i++) {
        printf("%02x", packet->receive_timestamp[i]);
    }
    printf("\n");

    // Print the transmit timestamp
    printf("Transmit timestamp: ");
    for (int i = 0; i < 8; i++) {
        printf("%02x", packet->transmit_timestamp[i]);
    }
    printf("\n");
}

int main() {
    // Example NTP packet data
    uint8_t data[] = {
        0x24, 0x03, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    };

    // Parse the NTP packet
    parse_ntp_packet(data, sizeof(data));

    return 0;
}