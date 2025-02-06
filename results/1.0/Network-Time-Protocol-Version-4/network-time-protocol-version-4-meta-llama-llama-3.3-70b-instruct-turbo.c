#include <hammer/hammer.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NTP_EPOCH_SECS 2208988800ULL

#define NTP_VN 4
#define NTP_MODE_CLIENT 3
#define NTP_MODE_SERVER 4

#define NTPExtensionFieldieiMac 0x0001
#define NTPExtensionFieldieInterleaved 0x0002

#define FLAG_LEAP 0xC0
#define FLAG_VERSION 0x38
#define FLAG_MODE 0x07

typedef struct {
    uint8_t li : 2;
    uint8_t vn : 3;
    uint8_t mode : 3;
    uint8_t poll;
    int8_t precision;
    uint32_t delay;
    uint32_t dispersion;
    uint32_t identifier;
    uint32_t reference_timestamp_s;
    uint32_t reference_timestamp_f;
    uint32_t origin_timestamp_s;
    uint32_t origin_timestamp_f;
    uint32_t receive_timestamp_s;
    uint32_t receive_timestamp_f;
    uint32_t transmit_timestamp_s;
    uint32_t transmit_timestamp_f;
} NTPHeader;

typedef struct {
    uint16_t type;
    uint16_t length;
    uint8_t* value;
} NTPExtensionField;

typedef struct {
    NTPHeader header;
    NTPExtensionField* extensions;
    int num_extensions;
} NTPPacket;

void print_ntp_packet(NTPPacket* packet) {
    printf("Leap Indicator: %d\n", packet->header.li);
    printf("Version Number: %d\n", packet->header.vn);
    printf("Mode: %d\n", packet->header.mode);
    printf("Poll: %d\n", packet->header.poll);
    printf("Precision: %d\n", packet->header.precision);
    printf("Delay: %u\n", packet->header.delay);
    printf("Dispersion: %u\n", packet->header.dispersion);
    printf("Identifier: %u\n", packet->header.identifier);
    printf("Reference Timestamp: %u.%u\n", packet->header.reference_timestamp_s, packet->header.reference_timestamp_f);
    printf("Origin Timestamp: %u.%u\n", packet->header.origin_timestamp_s, packet->header.origin_timestamp_f);
    printf("Receive Timestamp: %u.%u\n", packet->header.receive_timestamp_s, packet->header.receive_timestamp_f);
    printf("Transmit Timestamp: %u.%u\n", packet->header.transmit_timestamp_s, packet->header.transmit_timestamp_f);
    printf("Number of Extensions: %d\n", packet->num_extensions);
    for(int i = 0; i < packet->num_extensions; i++) {
        printf("Extension Field %d Type: %u\n", i, packet->extensions[i].type);
        printf("Extension Field %d Length: %u\n", i, packet->extensions[i].length);
        printf("Extension Field %d Value: ", i);
        for(int j = 0; j < packet->extensions[i].length; j++) {
            printf("%02x ", packet->extensions[i].value[j]);
        }
        printf("\n");
    }
}

int main(int argc, char* argv[]) {
    if(argc != 2) {
        printf("Usage: %s <filename>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if(!file) {
        printf("Failed to open file %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if(!buffer) {
        printf("Failed to allocate memory\n");
        return 1;
    }

    size_t bytes_read = fread(buffer, 1, file_size, file);
    if(bytes_read != file_size) {
        printf("Failed to read file %s\n", argv[1]);
        return 1;
    }

    fclose(file);

    NTPPacket packet;
    hammer_parse(ntp_packet, buffer, bytes_read, &packet);
    print_ntp_packet(&packet);

    free(buffer);
    return 0;
}