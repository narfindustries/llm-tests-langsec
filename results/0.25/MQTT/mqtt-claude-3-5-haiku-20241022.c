#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>

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
    DISCONNECT = 14
} MQTTPacketType;

typedef struct {
    MQTTPacketType type;
    uint8_t flags;
    HParser* parser;
} MQTTPacket;

HParser* mqtt_protocol_name() {
    return h_sequence(
        h_ch('M'), h_ch('Q'), h_ch('T'), h_ch('T'),
        NULL
    );
}

HParser* mqtt_variable_length_integer() {
    return h_many1(h_uint8());
}

HParser* mqtt_utf8_string() {
    HParser* length = h_uint16();
    HParser* string = h_length_value(length, h_ch_range(0x20, 0x7E));
    return string;
}

HParser* mqtt_properties() {
    return h_optional(
        h_sequence(
            h_uint8(),  // Property length
            h_many(
                h_choice(
                    h_uint8(),  // Property identifier
                    h_choice(
                        mqtt_utf8_string(),
                        h_uint32(),
                        h_uint16(),
                        NULL
                    ),
                    NULL
                )
            ),
            NULL
        )
    );
}

HParser* mqtt_connect_parser() {
    return h_sequence(
        mqtt_protocol_name(),
        h_uint8(),  // Protocol version
        h_bits(8, false),  // Connect flags
        h_uint16(),  // Keep alive
        mqtt_properties(),
        mqtt_utf8_string(),  // Client ID
        h_optional(mqtt_utf8_string()),  // Username
        h_optional(mqtt_utf8_string()),  // Password
        NULL
    );
}

HParser* mqtt_publish_parser() {
    return h_sequence(
        mqtt_utf8_string(),  // Topic name
        h_optional(h_uint16()),  // Packet identifier
        mqtt_properties(),
        h_many(h_uint8()),  // Payload
        NULL
    );
}

HParser* mqtt_packet_parser() {
    return h_choice(
        mqtt_connect_parser(),
        mqtt_publish_parser(),
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

    HParser* parser = mqtt_packet_parser();
    HParseResult* result = h_parse(parser, buffer, read_size);

    if (result && result->ast) {
        printf("MQTT packet parsed successfully\n");
    } else {
        printf("MQTT packet parsing failed\n");
    }

    h_parse_result_free(result);
    h_parser_free(parser);
    free(buffer);

    return 0;
}