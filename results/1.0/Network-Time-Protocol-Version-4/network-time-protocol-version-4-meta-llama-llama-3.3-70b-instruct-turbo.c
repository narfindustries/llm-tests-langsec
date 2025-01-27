#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define the structure for Network Time Protocol Version 4
typedef struct {
    uint8_t leap_indicator;
    uint8_t version;
    uint8_t mode;
    uint8_t stratum;
    uint8_t poll;
    uint8_t precision;
    uint32_t root_delay;
    uint32_t root_dispersion;
    uint32_t reference_identifier;
    uint32_t reference_timestamp_seconds;
    uint32_t reference_timestamp_fraction;
    uint32_t originate_timestamp_seconds;
    uint32_t originate_timestamp_fraction;
    uint32_t receive_timestamp_seconds;
    uint32_t receive_timestamp_fraction;
    uint32_t transmit_timestamp_seconds;
    uint32_t transmit_timestamp_fraction;
} ntp_packet_t;

// Define the structure for Extension Fields
typedef struct {
    uint16_t field_type;
    uint16_t field_length;
    uint8_t* field_value;
} extension_field_t;

// Function to parse NTP packet
ntp_packet_t* parse_ntp_packet(uint8_t* buffer, size_t length) {
    ntp_packet_t* packet = (ntp_packet_t*) malloc(sizeof(ntp_packet_t));
    if (packet == NULL) {
        return NULL;
    }

    // Parse the NTP packet
    packet->leap_indicator = (buffer[0] >> 6) & 0x3;
    packet->version = (buffer[0] >> 3) & 0x7;
    packet->mode = buffer[0] & 0x7;
    packet->stratum = buffer[1];
    packet->poll = buffer[2];
    packet->precision = buffer[3];
    packet->root_delay = (uint32_t) (((uint32_t) buffer[4] << 24) | ((uint32_t) buffer[5] << 16) | ((uint32_t) buffer[6] << 8) | (uint32_t) buffer[7]);
    packet->root_dispersion = (uint32_t) (((uint32_t) buffer[8] << 24) | ((uint32_t) buffer[9] << 16) | ((uint32_t) buffer[10] << 8) | (uint32_t) buffer[11]);
    packet->reference_identifier = (uint32_t) (((uint32_t) buffer[12] << 24) | ((uint32_t) buffer[13] << 16) | ((uint32_t) buffer[14] << 8) | (uint32_t) buffer[15]);
    packet->reference_timestamp_seconds = (uint32_t) (((uint32_t) buffer[16] << 24) | ((uint32_t) buffer[17] << 16) | ((uint32_t) buffer[18] << 8) | (uint32_t) buffer[19]);
    packet->reference_timestamp_fraction = (uint32_t) (((uint32_t) buffer[20] << 24) | ((uint32_t) buffer[21] << 16) | ((uint32_t) buffer[22] << 8) | (uint32_t) buffer[23]);
    packet->originate_timestamp_seconds = (uint32_t) (((uint32_t) buffer[24] << 24) | ((uint32_t) buffer[25] << 16) | ((uint32_t) buffer[26] << 8) | (uint32_t) buffer[27]);
    packet->originate_timestamp_fraction = (uint32_t) (((uint32_t) buffer[28] << 24) | ((uint32_t) buffer[29] << 16) | ((uint32_t) buffer[30] << 8) | (uint32_t) buffer[31]);
    packet->receive_timestamp_seconds = (uint32_t) (((uint32_t) buffer[32] << 24) | ((uint32_t) buffer[33] << 16) | ((uint32_t) buffer[34] << 8) | (uint32_t) buffer[35]);
    packet->receive_timestamp_fraction = (uint32_t) (((uint32_t) buffer[36] << 24) | ((uint32_t) buffer[37] << 16) | ((uint32_t) buffer[38] << 8) | (uint32_t) buffer[39]);
    packet->transmit_timestamp_seconds = (uint32_t) (((uint32_t) buffer[40] << 24) | ((uint32_t) buffer[41] << 16) | ((uint32_t) buffer[42] << 8) | (uint32_t) buffer[43]);
    packet->transmit_timestamp_fraction = (uint32_t) (((uint32_t) buffer[44] << 24) | ((uint32_t) buffer[45] << 16) | ((uint32_t) buffer[46] << 8) | (uint32_t) buffer[47]);

    return packet;
}

// Function to free NTP packet
void free_ntp_packet(ntp_packet_t* packet) {
    free(packet);
}

// Main function
int main() {
    // Example usage
    uint8_t buffer[] = {
        0x28, 0x01, 0x03, 0xe4, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    };

    ntp_packet_t* packet = parse_ntp_packet(buffer, sizeof(buffer));
    if (packet != NULL) {
        printf("Leap Indicator: %d\n", packet->leap_indicator);
        printf("Version: %d\n", packet->version);
        printf("Mode: %d\n", packet->mode);
        printf("Stratum: %d\n", packet->stratum);
        printf("Poll: %d\n", packet->poll);
        printf("Precision: %d\n", packet->precision);
        printf("Root Delay: %u\n", packet->root_delay);
        printf("Root Dispersion: %u\n", packet->root_dispersion);
        printf("Reference Identifier: %u\n", packet->reference_identifier);
        printf("Reference Timestamp Seconds: %u\n", packet->reference_timestamp_seconds);
        printf("Reference Timestamp Fraction: %u\n", packet->reference_timestamp_fraction);
        printf("Originate Timestamp Seconds: %u\n", packet->originate_timestamp_seconds);
        printf("Originate Timestamp Fraction: %u\n", packet->originate_timestamp_fraction);
        printf("Receive Timestamp Seconds: %u\n", packet->receive_timestamp_seconds);
        printf("Receive Timestamp Fraction: %u\n", packet->receive_timestamp_fraction);
        printf("Transmit Timestamp Seconds: %u\n", packet->transmit_timestamp_seconds);
        printf("Transmit Timestamp Fraction: %u\n", packet->transmit_timestamp_fraction);

        free_ntp_packet(packet);
    }

    return 0;
}