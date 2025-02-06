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
    double root_delay;
    double root_dispersion;
    char reference_id[5];
    uint64_t reference_timestamp;
    uint64_t origin_timestamp;
    uint64_t receive_timestamp;
    uint64_t transmit_timestamp;
} NTPPacket;

static HParser* ntp_parser;

static HParseResult* parse_ntp_packet(void* data, size_t len) {
    return h_parse(ntp_parser, data, len);
}

static void print_ntp_packet(NTPPacket* packet) {
    printf("NTP Packet Details:\n");
    printf("Leap Indicator: %d\n", packet->leap_indicator);
    printf("Version: %d\n", packet->version);
    printf("Mode: %d\n", packet->mode);
    printf("Stratum: %d\n", packet->stratum);
    printf("Poll Interval: %d\n", packet->poll_interval);
    printf("Precision: %d\n", packet->precision);
    printf("Reference ID: %s\n", packet->reference_id);
}

void initialize_ntp_parser() {
    HParser* leap_indicator = h_bits(2, false);
    HParser* version = h_bits(3, false);
    HParser* mode = h_bits(3, false);
    HParser* stratum = h_bits(8, false);
    HParser* poll = h_int8();
    HParser* precision = h_int8();
    HParser* root_delay = h_float_be();
    HParser* root_dispersion = h_float_be();
    HParser* reference_id = h_sequence(
        h_bits(8, false),
        h_bits(8, false),
        h_bits(8, false),
        h_bits(8, false),
        NULL
    );
    HParser* timestamp = h_bits(64, false);

    ntp_parser = h_sequence(
        leap_indicator,
        version,
        mode,
        stratum,
        poll,
        precision,
        root_delay,
        root_dispersion,
        reference_id,
        timestamp,
        timestamp,
        timestamp,
        timestamp,
        NULL
    );
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

    size_t read_size = fread(buffer, 1, file_size, file);
    fclose(file);

    if (read_size != file_size) {
        perror("File read error");
        free(buffer);
        return 1;
    }

    initialize_ntp_parser();

    HParseResult* result = parse_ntp_packet(buffer, read_size);
    if (result && result->ast) {
        NTPPacket packet;
        const HParsedToken* tokens = result->ast;

        packet.leap_indicator = tokens[0].uint;
        packet.version = tokens[1].uint;
        packet.mode = tokens[2].uint;
        packet.stratum = tokens[3].uint;
        packet.poll_interval = tokens[4].sint;
        packet.precision = tokens[5].sint;
        packet.root_delay = tokens[6].flt;
        packet.root_dispersion = tokens[7].flt;

        const HCountedArray* ref_id_tokens = tokens[8].seq;
        for (int i = 0; i < 4; i++) {
            packet.reference_id[i] = ref_id_tokens->elements[i].uint;
        }
        packet.reference_id[4] = '\0';

        packet.reference_timestamp = tokens[9].uint;
        packet.origin_timestamp = tokens[10].uint;
        packet.receive_timestamp = tokens[11].uint;
        packet.transmit_timestamp = tokens[12].uint;

        print_ntp_packet(&packet);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Parsing failed\n");
    }

    free(buffer);
    return 0;
}