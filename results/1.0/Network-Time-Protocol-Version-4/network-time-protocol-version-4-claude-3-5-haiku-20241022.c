#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

typedef struct {
    uint8_t leap_indicator;
    uint8_t version;
    uint8_t mode;
    uint8_t stratum;
    int8_t poll_interval;
    int8_t precision;
    uint32_t root_delay;
    uint32_t root_dispersion;
    uint32_t reference_id;
    uint64_t reference_timestamp;
    uint64_t originate_timestamp;
    uint64_t receive_timestamp;
    uint64_t transmit_timestamp;
} NTPPacket;

HParser* build_ntp_parser() {
    return h_sequence(
        h_bits(2, false),   // leap_indicator
        h_bits(3, false),   // version
        h_bits(3, false),   // mode
        h_bits(8, false),   // stratum
        h_int8(),           // poll_interval
        h_int8(),           // precision
        h_uint32(),         // root_delay
        h_uint32(),         // root_dispersion
        h_uint32(),         // reference_id
        h_uint64(),         // reference_timestamp
        h_uint64(),         // originate_timestamp
        h_uint64(),         // receive_timestamp
        h_uint64(),         // transmit_timestamp
        NULL
    );
}

void print_ntp_packet(NTPPacket* packet) {
    printf("Leap Indicator: %u\n", packet->leap_indicator);
    printf("Version: %u\n", packet->version);
    printf("Mode: %u\n", packet->mode);
    printf("Stratum: %u\n", packet->stratum);
    printf("Poll Interval: %d\n", packet->poll_interval);
    printf("Precision: %d\n", packet->precision);
    printf("Root Delay: %u\n", packet->root_delay);
    printf("Root Dispersion: %u\n", packet->root_dispersion);
    printf("Reference ID: %u\n", packet->reference_id);
    printf("Reference Timestamp: %lu\n", packet->reference_timestamp);
    printf("Originate Timestamp: %lu\n", packet->originate_timestamp);
    printf("Receive Timestamp: %lu\n", packet->receive_timestamp);
    printf("Transmit Timestamp: %lu\n", packet->transmit_timestamp);
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <ntp_packet_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("Error reading file");
        free(buffer);
        fclose(file);
        return 1;
    }

    HParser* parser = build_ntp_parser();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result && result->ast) {
        NTPPacket* packet = (NTPPacket*)result->ast;
        print_ntp_packet(packet);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse NTP packet\n");
    }

    free(buffer);
    fclose(file);
    return 0;
}