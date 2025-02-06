#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
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
} NTPv4_Packet;

static HParser* parse_ntp_header() {
    HParser* leap_indicator = h_bits(h_int_range(h_ch('0'), 0, 3), 2);
    HParser* version = h_bits(h_int_range(h_ch('1'), 1, 4), 3);
    HParser* mode = h_bits(h_int_range(h_ch('0'), 0, 7), 3);
    HParser* stratum = h_bits(h_int_range(h_ch(0), 0, 255), 8);
    HParser* poll_interval = h_int8();
    HParser* precision = h_int8();
    HParser* root_delay = h_double();
    HParser* root_dispersion = h_double();
    HParser* reference_id = h_sequence(h_ch(0), h_ch(0), h_ch(0), h_ch(0), NULL);

    return h_sequence(
        leap_indicator,
        version,
        mode,
        stratum,
        poll_interval,
        precision,
        root_delay,
        root_dispersion,
        reference_id,
        NULL
    );
}

static HParser* parse_ntp_timestamps() {
    HParser* timestamp = h_uint64();
    return h_sequence(
        timestamp,  // reference
        timestamp,  // origin
        timestamp,  // receive
        timestamp,  // transmit
        NULL
    );
}

NTPv4_Packet* parse_ntp_packet(const uint8_t* data, size_t len) {
    NTPv4_Packet* packet = malloc(sizeof(NTPv4_Packet));
    
    HParser* header_parser = parse_ntp_header();
    HParser* timestamp_parser = parse_ntp_timestamps();

    HParseResult* header_result = h_parse(header_parser, data, len);
    HParseResult* timestamp_result = h_parse(timestamp_parser, data + 32, len - 32);

    if (header_result && timestamp_result) {
        HParsedToken* header_tokens = header_result->ast;
        HParsedToken* timestamp_tokens = timestamp_result->ast;

        packet->leap_indicator = header_tokens->children[0]->uint;
        packet->version = header_tokens->children[1]->uint;
        packet->mode = header_tokens->children[2]->uint;
        packet->stratum = header_tokens->children[3]->uint;
        packet->poll_interval = header_tokens->children[4]->sint;
        packet->precision = header_tokens->children[5]->sint;
        packet->root_delay = header_tokens->children[6]->dbl;
        packet->root_dispersion = header_tokens->children[7]->dbl;

        const char* ref_id = (const char*)header_tokens->children[8]->str;
        memcpy(packet->reference_id, ref_id, 4);
        packet->reference_id[4] = '\0';

        packet->reference_timestamp = timestamp_tokens->children[0]->uint;
        packet->origin_timestamp = timestamp_tokens->children[1]->uint;
        packet->receive_timestamp = timestamp_tokens->children[2]->uint;
        packet->transmit_timestamp = timestamp_tokens->children[3]->uint;

        h_parse_result_free(header_result);
        h_parse_result_free(timestamp_result);

        return packet;
    }

    free(packet);
    return NULL;
}

void print_ntp_packet(NTPv4_Packet* packet) {
    printf("NTP Packet Details:\n");
    printf("Leap Indicator: %d\n", packet->leap_indicator);
    printf("Version: %d\n", packet->version);
    printf("Mode: %d\n", packet->mode);
    printf("Stratum: %d\n", packet->stratum);
    printf("Poll Interval: %d\n", packet->poll_interval);
    printf("Precision: %d\n", packet->precision);
    printf("Reference ID: %s\n", packet->reference_id);
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <ntp_file>\n", argv[0]);
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

    size_t bytes_read = fread(buffer, 1, file_size, file);
    fclose(file);

    if (bytes_read != file_size) {
        perror("Error reading file");
        free(buffer);
        return 1;
    }

    NTPv4_Packet* packet = parse_ntp_packet(buffer, file_size);
    if (packet) {
        print_ntp_packet(packet);
        free(packet);
    } else {
        fprintf(stderr, "Failed to parse NTP packet\n");
    }

    free(buffer);
    return 0;
}