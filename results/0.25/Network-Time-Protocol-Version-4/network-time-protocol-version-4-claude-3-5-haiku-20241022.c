#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef struct {
    uint8_t leap_indicator;
    uint8_t version;
    uint8_t mode;
    uint8_t stratum;
    int8_t poll_interval;
    int8_t precision;
    double root_delay;
    double root_dispersion;
    char reference_id[5];
    uint64_t reference_timestamp;
    uint64_t origin_timestamp;
    uint64_t receive_timestamp;
    uint64_t transmit_timestamp;
} NTPPacket;

HParser* parse_ntp_packet() {
    HParser* leap_indicator = h_bits(h_ch_range(0, 3), 2);
    HParser* version = h_bits(h_ch_range(1, 4), 3);
    HParser* mode = h_bits(h_ch_range(0, 7), 3);
    HParser* stratum = h_bits(h_ch_range(0, 255), 8);
    HParser* poll_interval = h_bits(h_ch_range(-6, 27), 8);
    HParser* precision = h_bits(h_ch_range(-128, 127), 8);
    HParser* root_delay = h_bits(h_ch_range(0, UINT32_MAX), 32);
    HParser* root_dispersion = h_bits(h_ch_range(0, UINT32_MAX), 32);
    HParser* reference_id = h_bits(h_ch_range(0, UINT32_MAX), 32);
    HParser* reference_timestamp = h_bits(h_ch_range(0, UINT64_MAX), 64);
    HParser* origin_timestamp = h_bits(h_ch_range(0, UINT64_MAX), 64);
    HParser* receive_timestamp = h_bits(h_ch_range(0, UINT64_MAX), 64);
    HParser* transmit_timestamp = h_bits(h_ch_range(0, UINT64_MAX), 64);

    return h_sequence(
        leap_indicator, version, mode, stratum,
        poll_interval, precision, root_delay, root_dispersion,
        reference_id, reference_timestamp, origin_timestamp,
        receive_timestamp, transmit_timestamp, NULL
    );
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

    size_t read_size = fread(buffer, 1, file_size, file);
    fclose(file);

    if (read_size != file_size) {
        perror("File read error");
        free(buffer);
        return 1;
    }

    HParser* parser = parse_ntp_packet();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (!result || !result->ast) {
        fprintf(stderr, "Parsing failed\n");
        free(buffer);
        h_parse_result_free(result);
        return 1;
    }

    printf("NTP Packet Parsed Successfully\n");

    h_parse_result_free(result);
    free(buffer);
    return 0;
}