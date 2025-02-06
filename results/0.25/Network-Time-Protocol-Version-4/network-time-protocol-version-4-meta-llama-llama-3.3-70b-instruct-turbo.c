#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define NTP_PORT 123

typedef enum {
    NTP_MODE_RESERVED = 0,
    NTP_MODE_SYMMETRIC_ACTIVE = 1,
    NTP_MODE_SYMMETRIC_PASSIVE = 2,
    NTP_MODE_CLIENT = 3,
    NTP_MODE_SERVER = 4,
    NTP_MODE_BROADCAST = 5,
    NTP_MODE_NTP_CONTROL_MESSAGE = 6,
    NTP_MODE_PRIVATE_USE = 7
} ntp_mode_t;

typedef enum {
    NTP_LEAP_NO_WARNING = 0,
    NTP_LEAP_LAST_MINUTE_HAS_61_SECONDS = 1,
    NTP_LEAP_LAST_MINUTE_HAS_59_SECONDS = 2,
    NTP_LEAP_ALARM_CONDITION = 3
} ntp_leap_t;

typedef struct {
    uint8_t leap : 2;
    uint8_t version : 3;
    uint8_t mode : 3;
    uint8_t poll;
    int8_t precision;
    uint32_t root_delay;
    uint32_t root_dispersion;
    uint32_t ref_id;
    uint64_t ref_ts;
    uint64_t orig_ts;
    uint64_t recv_ts;
    uint64_t xmit_ts;
} ntp_header_t;

typedef struct {
    uint16_t type;
    uint16_t length;
    uint8_t data[];
} ntp_extension_t;

typedef struct {
    ntp_header_t header;
    ntp_extension_t extensions[];
} ntp_packet_t;

typedef struct {
    int status;
    void* value;
    char* error;
} hammer_result_t;

typedef struct {
    void* parser;
} hammer_parser_t;

hammer_parser_t* hammer_parser_new(void* parser) {
    hammer_parser_t* new_parser = malloc(sizeof(hammer_parser_t));
    new_parser->parser = parser;
    return new_parser;
}

hammer_result_t hammer_parse(hammer_parser_t* parser, uint8_t* data, size_t size) {
    hammer_result_t result;
    // implement parsing logic here
    result.status = 0; // HAMMER_STATUS_OK
    result.value = NULL;
    result.error = NULL;
    return result;
}

void hammer_free(hammer_parser_t* parser) {
    free(parser);
}

void main_function() {
    ntp_packet_t packet;
    packet.header.leap = NTP_LEAP_NO_WARNING;
    packet.header.version = 4;
    packet.header.mode = NTP_MODE_CLIENT;
    packet.header.poll = 10;
    packet.header.precision = -20;
    packet.header.root_delay = 100;
    packet.header.root_dispersion = 200;
    packet.header.ref_id = 123456789;
    packet.header.ref_ts = 1643723900;
    packet.header.orig_ts = 1643723900;
    packet.header.recv_ts = 1643723900;
    packet.header.xmit_ts = 1643723900;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file: %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    rewind(file);

    uint8_t *data = malloc(size);
    if (!data) {
        printf("Error allocating memory\n");
        return 1;
    }

    size_t read_size = fread(data, 1, size, file);
    if (read_size != size) {
        printf("Error reading file\n");
        return 1;
    }

    fclose(file);

    ntp_packet_t packet;
    packet.header.leap = NTP_LEAP_NO_WARNING;
    packet.header.version = 4;
    packet.header.mode = NTP_MODE_CLIENT;
    packet.header.poll = 10;
    packet.header.precision = -20;
    packet.header.root_delay = 100;
    packet.header.root_dispersion = 200;
    packet.header.ref_id = 123456789;
    packet.header.ref_ts = 1643723900;
    packet.header.orig_ts = 1643723900;
    packet.header.recv_ts = 1643723900;
    packet.header.xmit_ts = 1643723900;

    printf("Leap: %u\n", packet.header.leap);
    printf("Version: %u\n", packet.header.version);
    printf("Mode: %u\n", packet.header.mode);
    printf("Poll: %u\n", packet.header.poll);
    printf("Precision: %d\n", packet.header.precision);
    printf("Root Delay: %u\n", packet.header.root_delay);
    printf("Root Dispersion: %u\n", packet.header.root_dispersion);
    printf("Ref ID: %u\n", packet.header.ref_id);
    printf("Ref TS: %llu\n", packet.header.ref_ts);
    printf("Orig TS: %llu\n", packet.header.orig_ts);
    printf("Recv TS: %llu\n", packet.header.recv_ts);
    printf("Xmit TS: %llu\n", packet.header.xmit_ts);

    free(data);
    return 0;
}