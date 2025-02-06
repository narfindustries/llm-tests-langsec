#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <arpa/inet.h>

typedef struct {
    uint32_t seconds;
    uint32_t fraction;
} ntp_timestamp_t;

typedef struct {
    uint8_t li_vn_mode;
    uint8_t stratum;
    int8_t poll;
    int8_t precision;
    uint32_t root_delay;
    uint32_t root_dispersion;
    uint32_t reference_id;
    ntp_timestamp_t reference_timestamp;
    ntp_timestamp_t originate_timestamp;
    ntp_timestamp_t receive_timestamp;
    ntp_timestamp_t transmit_timestamp;
    // Add other optional fields as needed...
} ntp_packet_t;

hammer_parser ntp_timestamp_parser() {
    return hammer_map2(
        &(ntp_timestamp_t){},
        hammer_seq(hammer_uint32_be(), hammer_uint32_be())
    );
}

hammer_parser ntp_packet_parser() {
    return hammer_map11(
        &(ntp_packet_t){},
        hammer_seq(hammer_uint8(), hammer_uint8(), hammer_int8(), hammer_int8(), hammer_uint32_be(), hammer_uint32_be(), hammer_uint32_be(), ntp_timestamp_parser(), ntp_timestamp_parser(), ntp_timestamp_parser(), ntp_timestamp_parser())
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <ntp_binary_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char *buffer = (char *)malloc(fsize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }
    fread(buffer, 1, fsize, fp);
    fclose(fp);

    hammer_result result = hammer_parse(ntp_packet_parser(), buffer, fsize);

    if (result.success) {
        ntp_packet_t packet = *(ntp_packet_t *)result.value;
        printf("NTP Packet:\n");
        printf("LI_VN_Mode: 0x%02X\n", packet.li_vn_mode);
        printf("Stratum: %u\n", packet.stratum);
        printf("Poll: %d\n", packet.poll);
        printf("Precision: %d\n", packet.precision);
        printf("Root Delay: %u\n", packet.root_delay);
        printf("Root Dispersion: %u\n", packet.root_dispersion);
        printf("Reference ID: %u\n", packet.reference_id);
        printf("Reference Timestamp: %u.%u\n", packet.reference_timestamp.seconds, packet.reference_timestamp.fraction);
        printf("Originate Timestamp: %u.%u\n", packet.originate_timestamp.seconds, packet.originate_timestamp.fraction);
        printf("Receive Timestamp: %u.%u\n", packet.receive_timestamp.seconds, packet.receive_timestamp.fraction);
        printf("Transmit Timestamp: %u.%u\n", packet.transmit_timestamp.seconds, packet.transmit_timestamp.fraction);

        free(result.value);
    } else {
        fprintf(stderr, "Error parsing NTP packet: %s\n", result.error);
    }

    free(buffer);
    return 0;
}
