#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

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
    HParser* protocol_name;
    uint8_t protocol_version;
    struct {
        uint8_t username_flag;
        uint8_t password_flag;
        uint8_t will_retain;
        uint8_t will_qos;
        uint8_t will_flag;
        uint8_t clean_start;
    } connect_flags;
    HParser* properties;
    HParser* payload;
} MQTTPacket;

HParser* mqtt_protocol_name() {
    return h_literal("MQTT");
}

HParser* mqtt_fixed_header() {
    return h_sequence(
        h_bits(4, false),  // Packet Type
        h_bits(4, false),  // Flags
        NULL
    );
}

HParser* mqtt_connect_flags() {
    return h_bits(8, false);
}

HParser* mqtt_properties() {
    return h_choice(
        h_uint8(),   // Property Length
        h_sequence( // Variable Properties
            h_uint8(),  // Property Type
            h_uint8(),  // Property Value
            NULL
        ),
        NULL
    );
}

HParser* mqtt_parser() {
    return h_sequence(
        mqtt_fixed_header(),
        mqtt_protocol_name(),
        h_uint8(),           // Protocol Version
        mqtt_connect_flags(),
        mqtt_properties(),
        h_end_p(),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <mqtt_binary_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation failed");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser* parser = mqtt_parser();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result && result->ast) {
        printf("MQTT Packet parsed successfully\n");
    } else {
        printf("MQTT Packet parsing failed\n");
    }

    h_parse_result_free(result);
    h_destroy_parser(parser);
    free(buffer);

    return 0;
}