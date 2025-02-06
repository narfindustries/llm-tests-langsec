#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

// Define MQTT types and constants
typedef enum {
    MQTT_CONNECT = 1,
    MQTT_CONNACK,
    MQTT_PUBLISH,
    MQTT_PUBACK,
    MQTT_PUBREC,
    MQTT_PUBREL,
    MQTT_PUBCOMP,
    MQTT_SUBSCRIBE,
    MQTT_SUBACK,
    MQTT_UNSUBSCRIBE,
    MQTT_UNSUBACK,
    MQTT_PINGREQ,
    MQTT_PINGRESP,
    MQTT_DISCONNECT,
    MQTT_AUTH
} MQTT_PacketType;

typedef enum {
    MQTT_QOS_0 = 0,
    MQTT_QOS_1,
    MQTT_QOS_2
} MQTT_QoS;

typedef struct {
    uint8_t type;
    uint8_t flags;
    uint32_t remaining_length;
    uint8_t *payload;
} MQTT_Packet;

void parse_mqtt_packet(const uint8_t *data, size_t size) {
    HParser *mqtt_packet_type_parser = h_uint8();
    HParser *mqtt_flags_parser = h_uint8();
    HParser *mqtt_remaining_length_parser = h_uint32();
    HParser *mqtt_payload_parser = h_many(h_uint8());

    HParser *mqtt_packet_parser = h_sequence(
        mqtt_packet_type_parser, mqtt_flags_parser,
        mqtt_remaining_length_parser, mqtt_payload_parser,
        NULL
    );

    HParseResult *result = h_parse(mqtt_packet_parser, data, size);
    if (result) {
        MQTT_Packet *packet = (MQTT_Packet *)result->ast;
        printf("Packet Type: %d\n", packet->type);
        printf("Flags: %d\n", packet->flags);
        printf("Remaining Length: %d\n", packet->remaining_length);
        printf("Payload: ");
        for (uint32_t i = 0; i < packet->remaining_length; i++) {
            printf("%02x ", packet->payload[i]);
        }
        printf("\n");
    } else {
        printf("Failed to parse MQTT packet.\n");
    }
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = (uint8_t *)malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    parse_mqtt_packet(data, file_size);
    free(data);

    return 0;
}