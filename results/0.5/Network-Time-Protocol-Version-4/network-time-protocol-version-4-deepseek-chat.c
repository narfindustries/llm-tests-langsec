#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    uint8_t leap_indicator;
    uint8_t version_number;
    uint8_t mode;
    uint8_t stratum;
    uint8_t poll;
    uint8_t precision;
    uint32_t root_delay;
    uint32_t root_dispersion;
    uint32_t reference_identifier;
    uint64_t reference_timestamp;
    uint64_t originate_timestamp;
    uint64_t receive_timestamp;
    uint64_t transmit_timestamp;
} ntp_packet;

HParser *ntp_parser() {
    HParser *leap_indicator = h_uint8();
    HParser *version_number = h_uint8();
    HParser *mode = h_uint8();
    HParser *stratum = h_uint8();
    HParser *poll = h_uint8();
    HParser *precision = h_uint8();
    HParser *root_delay = h_uint32();
    HParser *root_dispersion = h_uint32();
    HParser *reference_identifier = h_uint32();
    HParser *reference_timestamp = h_uint64();
    HParser *originate_timestamp = h_uint64();
    HParser *receive_timestamp = h_uint64();
    HParser *transmit_timestamp = h_uint64();

    return h_sequence(
        leap_indicator, version_number, mode, stratum, poll, precision,
        root_delay, root_dispersion, reference_identifier,
        reference_timestamp, originate_timestamp,
        receive_timestamp, transmit_timestamp,
        NULL
    );
}

int main() {
    HParser *parser = ntp_parser();
    uint8_t input[] = {
        0x1B, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    };
    size_t input_size = sizeof(input);

    HParseResult *result = h_parse(parser, input, input_size);
    if (result) {
        ntp_packet *packet = (ntp_packet *)result->ast;
        printf("Leap Indicator: %d\n", packet->leap_indicator);
        printf("Version Number: %d\n", packet->version_number);
        printf("Mode: %d\n", packet->mode);
        printf("Stratum: %d\n", packet->stratum);
        printf("Poll: %d\n", packet->poll);
        printf("Precision: %d\n", packet->precision);
        printf("Root Delay: %u\n", packet->root_delay);
        printf("Root Dispersion: %u\n", packet->root_dispersion);
        printf("Reference Identifier: %u\n", packet->reference_identifier);
        printf("Reference Timestamp: %lu\n", packet->reference_timestamp);
        printf("Originate Timestamp: %lu\n", packet->originate_timestamp);
        printf("Receive Timestamp: %lu\n", packet->receive_timestamp);
        printf("Transmit Timestamp: %lu\n", packet->transmit_timestamp);
        h_parse_result_free(result);
    } else {
        printf("Failed to parse NTP packet.\n");
    }

    return 0;
}