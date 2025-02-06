#include <hammer/hammer.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NTP_PACKET_SIZE 48

typedef enum {
    NTP_LEAP_NO_WARNING,
    NTP_LEAP_LAST_MINUTE_HAS_61_SECONDS,
    NTP_LEAP_LAST_MINUTE_HAS_59_SECONDS,
    NTP_LEAP_ALARM_CONDITION
} ntp_leap_indicator;

typedef enum {
    NTP_VERSION_4
} ntp_version;

typedef enum {
    NTP_MODE_RESERVED,
    NTP_MODE_SYMMETRIC_ACTIVE,
    NTP_MODE_SYMMETRIC_PASSIVE,
    NTP_MODE_CLIENT,
    NTP_MODE_SERVER,
    NTP_MODE_BROADCAST,
    NTP_MODE_RESERVED_FOR_NTP_CONTROL_MESSAGE,
    NTP_MODE_RESERVED_FOR_PRIVATE_USE
} ntp_mode;

typedef struct {
    ntp_leap_indicator leap_indicator : 2;
    ntp_version version : 3;
    ntp_mode mode : 3;
} ntp_header_flags;

typedef struct {
    uint8_t poll;
    uint8_t precision;
    uint32_t root_delay;
    uint32_t root_dispersion;
    uint32_t reference_clock_identifier;
    uint64_t reference_timestamp;
    uint64_t origin_timestamp;
    uint64_t receive_timestamp;
    uint64_t transmit_timestamp;
} ntp_header;

typedef struct {
    ntp_header_flags flags;
    ntp_header header;
} ntp_packet;

typedef struct {
    uint16_t type;
    uint16_t length;
    uint8_t* value;
} ntp_extension_field;

typedef struct {
    ntp_packet packet;
    ntp_extension_field* extension_fields;
    uint16_t num_extension_fields;
} ntp_message;

typedef struct hammer_env hammer_env_t;
typedef struct hammer_parser hammer_parser_t;
typedef enum hammer_result hammer_result_t;

hammer_parser_t* ntp_leap_indicator_parser(hammer_env_t* env) {
    hammer_parser_t* parser = h_choice(
        h_seq(
            h_lit(0),
            h_bind(ntp_leap_indicator, NTP_LEAP_NO_WARNING)
        ),
        h_seq(
            h_lit(1),
            h_bind(ntp_leap_indicator, NTP_LEAP_LAST_MINUTE_HAS_61_SECONDS)
        ),
        h_seq(
            h_lit(2),
            h_bind(ntp_leap_indicator, NTP_LEAP_LAST_MINUTE_HAS_59_SECONDS)
        ),
        h_seq(
            h_lit(3),
            h_bind(ntp_leap_indicator, NTP_LEAP_ALARM_CONDITION)
        )
    );
    return parser;
}

hammer_parser_t* ntp_version_parser(hammer_env_t* env) {
    hammer_parser_t* parser = h_seq(
        h_lit(4),
        h_bind(ntp_version, NTP_VERSION_4)
    );
    return parser;
}

hammer_parser_t* ntp_mode_parser(hammer_env_t* env) {
    hammer_parser_t* parser = h_choice(
        h_seq(
            h_lit(0),
            h_bind(ntp_mode, NTP_MODE_RESERVED)
        ),
        h_seq(
            h_lit(1),
            h_bind(ntp_mode, NTP_MODE_SYMMETRIC_ACTIVE)
        ),
        h_seq(
            h_lit(2),
            h_bind(ntp_mode, NTP_MODE_SYMMETRIC_PASSIVE)
        ),
        h_seq(
            h_lit(3),
            h_bind(ntp_mode, NTP_MODE_CLIENT)
        ),
        h_seq(
            h_lit(4),
            h_bind(ntp_mode, NTP_MODE_SERVER)
        ),
        h_seq(
            h_lit(5),
            h_bind(ntp_mode, NTP_MODE_BROADCAST)
        ),
        h_seq(
            h_lit(6),
            h_bind(ntp_mode, NTP_MODE_RESERVED_FOR_NTP_CONTROL_MESSAGE)
        ),
        h_seq(
            h_lit(7),
            h_bind(ntp_mode, NTP_MODE_RESERVED_FOR_PRIVATE_USE)
        )
    );
    return parser;
}

hammer_parser_t* ntp_header_flags_parser(hammer_env_t* env) {
    hammer_parser_t* parser = h_seq(
        h_bit_field(2, ntp_leap_indicator_parser(env)),
        h_bit_field(3, ntp_version_parser(env)),
        h_bit_field(3, ntp_mode_parser(env))
    );
    return parser;
}

hammer_parser_t* ntp_header_parser(hammer_env_t* env) {
    hammer_parser_t* parser = h_seq(
        h_byte() >> h_bind(uint8_t, poll),
        h_byte() >> h_bind(uint8_t, precision),
        h_uint32() >> h_bind(uint32_t, root_delay),
        h_uint32() >> h_bind(uint32_t, root_dispersion),
        h_uint32() >> h_bind(uint32_t, reference_clock_identifier),
        h_uint64() >> h_bind(uint64_t, reference_timestamp),
        h_uint64() >> h_bind(uint64_t, origin_timestamp),
        h_uint64() >> h_bind(uint64_t, receive_timestamp),
        h_uint64() >> h_bind(uint64_t, transmit_timestamp)
    );
    return parser;
}

hammer_parser_t* ntp_packet_parser(hammer_env_t* env) {
    hammer_parser_t* parser = h_seq(
        h_bind(ntp_header_flags, flags),
        h_bind(ntp_header, header)
    );
    return parser;
}

hammer_parser_t* ntp_extension_field_parser(hammer_env_t* env) {
    hammer_parser_t* parser = h_seq(
        h_uint16() >> h_bind(uint16_t, type),
        h_uint16() >> h_bind(uint16_t, length),
        h_bytes(length) >> h_bind(uint8_t*, value)
    );
    return parser;
}

hammer_parser_t* ntp_message_parser(hammer_env_t* env) {
    hammer_parser_t* parser = h_seq(
        h_bind(ntp_packet, packet),
        h_zero_or_more(ntp_extension_field_parser(env)) >> h_bind(ntp_extension_field*, extension_fields),
        h_length(extension_fields) >> h_bind(uint16_t, num_extension_fields)
    );
    return parser;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file %s\n", argv[1]);
        return 1;
    }

    uint8_t buffer[NTP_PACKET_SIZE];
    size_t bytes_read = fread(buffer, 1, NTP_PACKET_SIZE, file);
    if (bytes_read != NTP_PACKET_SIZE) {
        printf("Error reading file %s\n", argv[1]);
        return 1;
    }

    fclose(file);

    hammer_env_t env;
    h_init(&env);

    ntp_message message;
    hammer_result_t result = h_parse(ntp_message_parser(&env), buffer, NTP_PACKET_SIZE, &message);
    if (result != H_OK) {
        printf("Error parsing NTP message\n");
        return 1;
    }

    printf("Leap Indicator: %d\n", message.packet.flags.leap_indicator);
    printf("Version: %d\n", message.packet.flags.version);
    printf("Mode: %d\n", message.packet.flags.mode);
    printf("Poll: %d\n", message.packet.header.poll);
    printf("Precision: %d\n", message.packet.header.precision);
    printf("Root Delay: %u\n", message.packet.header.root_delay);
    printf("Root Dispersion: %u\n", message.packet.header.root_dispersion);
    printf("Reference Clock Identifier: %u\n", message.packet.header.reference_clock_identifier);
    printf("Reference Timestamp: %llu\n", message.packet.header.reference_timestamp);
    printf("Origin Timestamp: %llu\n", message.packet.header.origin_timestamp);
    printf("Receive Timestamp: %llu\n", message.packet.header.receive_timestamp);
    printf("Transmit Timestamp: %llu\n", message.packet.header.transmit_timestamp);

    for (uint16_t i = 0; i < message.num_extension_fields; i++) {
        printf("Extension Field %d:\n", i);
        printf("Type: %d\n", message.extension_fields[i].type);
        printf("Length: %d\n", message.extension_fields[i].length);
        printf("Value: ");
        for (uint16_t j = 0; j < message.extension_fields[i].length; j++) {
            printf("%02x", message.extension_fields[i].value[j]);
        }
        printf("\n");
    }

    return 0;
}