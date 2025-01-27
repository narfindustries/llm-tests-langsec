#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

// NTP packet structure
typedef struct {
    uint8_t leap;         // Leap indicator (2 bits) and version (3 bits) and mode (3 bits)
    uint8_t poll;         // Poll interval (8 bits)
    uint8_t precision;    // Precision (8 bits)
    uint32_t root_delay;  // Root delay (32 bits)
    uint32_t root_disp;   // Root dispersion (32 bits)
    uint32_t ref_id;      // Reference clock identifier (32 bits)
    uint64_t ref_timestamp;// Reference timestamp (64 bits)
    uint64_t orig_timestamp;// Origin timestamp (64 bits)
    uint64_t recv_timestamp;// Receive timestamp (64 bits)
    uint64_t trans_timestamp;// Transmit timestamp (64 bits)
} ntp_packet_t;

// Parsing function for NTP packet
ntp_packet_t parse_ntp_packet(const uint8_t* data, size_t length) {
    ntp_packet_t packet;

    if (length < 48) {
        printf("Invalid NTP packet length\n");
        return packet;
    }

    packet.leap = (data[0] >> 6) & 0x3;
    packet.poll = data[2];
    packet.precision = data[3];
    packet.root_delay = ((uint32_t)data[4] << 24) | ((uint32_t)data[5] << 16) | ((uint32_t)data[6] << 8) | (uint32_t)data[7];
    packet.root_disp = ((uint32_t)data[8] << 24) | ((uint32_t)data[9] << 16) | ((uint32_t)data[10] << 8) | (uint32_t)data[11];
    packet.ref_id = ((uint32_t)data[12] << 24) | ((uint32_t)data[13] << 16) | ((uint32_t)data[14] << 8) | (uint32_t)data[15];

    packet.ref_timestamp = ((uint64_t)data[16] << 56) | ((uint64_t)data[17] << 48) | ((uint64_t)data[18] << 40) | ((uint64_t)data[19] << 32) | ((uint64_t)data[20] << 24) | ((uint64_t)data[21] << 16) | ((uint64_t)data[22] << 8) | (uint64_t)data[23];

    packet.orig_timestamp = ((uint64_t)data[24] << 56) | ((uint64_t)data[25] << 48) | ((uint64_t)data[26] << 40) | ((uint64_t)data[27] << 32) | ((uint64_t)data[28] << 24) | ((uint64_t)data[29] << 16) | ((uint64_t)data[30] << 8) | (uint64_t)data[31];

    packet.recv_timestamp = ((uint64_t)data[32] << 56) | ((uint64_t)data[33] << 48) | ((uint64_t)data[34] << 40) | ((uint64_t)data[35] << 32) | ((uint64_t)data[36] << 24) | ((uint64_t)data[37] << 16) | ((uint64_t)data[38] << 8) | (uint64_t)data[39];

    packet.trans_timestamp = ((uint64_t)data[40] << 56) | ((uint64_t)data[41] << 48) | ((uint64_t)data[42] << 40) | ((uint64_t)data[43] << 32) | ((uint64_t)data[44] << 24) | ((uint64_t)data[45] << 16) | ((uint64_t)data[46] << 8) | (uint64_t)data[47];

    return packet;
}

int main() {
    uint8_t data[] = {
        0x28, 0x00, 0x06, 0xfa, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    };

    ntp_packet_t packet = parse_ntp_packet(data, sizeof(data));

    printf("Leap: %u\n", packet.leap);
    printf("Poll: %u\n", packet.poll);
    printf("Precision: %u\n", packet.precision);
    printf("Root Delay: %u\n", packet.root_delay);
    printf("Root Dispersion: %u\n", packet.root_disp);
    printf("Reference Clock Identifier: %u\n", packet.ref_id);
    printf("Reference Timestamp: %llu\n", packet.ref_timestamp);
    printf("Origin Timestamp: %llu\n", packet.orig_timestamp);
    printf("Receive Timestamp: %llu\n", packet.recv_timestamp);
    printf("Transmit Timestamp: %llu\n", packet.trans_timestamp);

    return 0;
}