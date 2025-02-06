#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef enum {
    CONNECT = 1,
    CONNACK = 2,
    PUBLISH = 3,
    PUBACK = 4,
    PUBREC = 5,
    PUBREL = 6,
    PUBCOMP = 7,
    SUBSCRIBE = 8,
    SUBACK = 9,
    UNSUBSCRIBE = 10,
    UNSUBACK = 11,
    PINGREQ = 12,
    PINGRESP = 13,
    DISCONNECT = 14,
    AUTH = 15
} MQTTPacketType;

typedef struct {
    uint8_t packet_type;
    uint8_t flags;
    HParseResult* remaining_length;
    HParseResult* properties;
    HParseResult* payload;
} MQTTPacket;

static HParser* mqtt_protocol_name() {
    return h_sequence(
        h_ch('M'), h_ch('Q'), h_ch('T'), h_ch('T'),
        NULL
    );
}

static HParser* mqtt_variable_length_integer() {
    return h_many1(
        h_sequence(
            h_bits(7, false),
            h_bits(1, false),
            NULL
        )
    );
}

static HParser* mqtt_properties() {
    return h_sequence(
        mqtt_variable_length_integer(),
        h_many(
            h_choice(
                h_int_range(h_uint8(), 1, 38),
                h_many1(h_bits(1, false)),
                NULL
            )
        ),
        NULL
    );
}

static HParser* mqtt_connect_packet() {
    return h_sequence(
        mqtt_protocol_name(),
        h_uint8(),
        h_bits(8, false),
        h_uint16(),
        mqtt_properties(),
        h_many1(h_bits(1, false)),
        NULL
    );
}

static HParser* mqtt_publish_packet() {
    return h_sequence(
        h_many1(h_bits(1, false)),
        h_choice(
            h_uint16(),
            h_epsilon_p(),
            NULL
        ),
        mqtt_properties(),
        h_many1(h_bits(1, false)),
        NULL
    );
}

static HParser* mqtt_packet() {
    return h_choice(
        mqtt_connect_packet(),
        mqtt_publish_packet(),
        h_many1(h_bits(1, false)),
        NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <mqtt_file>\n", argv[0]);
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
        perror("Memory allocation failed");
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

    HParser* parser = mqtt_packet();
    HParseResult* result = h_parse(parser, buffer, read_size);

    if (result) {
        printf("MQTT Packet parsed successfully\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "MQTT Packet parsing failed\n");
    }

    h_parser_destroy(parser);
    free(buffer);
    return 0;
}