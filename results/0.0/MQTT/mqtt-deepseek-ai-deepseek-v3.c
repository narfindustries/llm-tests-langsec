#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef struct {
    uint8_t packet_type;
    uint8_t flags;
    uint16_t packet_id;
    uint32_t properties_length;
    uint8_t *properties;
    uint8_t *payload;
} MQTT_Packet;

HParser *mqtt_packet_parser() {
    HParser *packet_type = h_uint8();
    HParser *flags = h_uint8();
    HParser *packet_id = h_uint16();
    HParser *properties_length = h_uint32();
    HParser *properties = h_length_value(h_uint32(), h_uint8());
    HParser *payload = h_many(h_uint8());

    return h_sequence(packet_type, flags, packet_id, properties_length, properties, payload, NULL);
}

MQTT_Packet *parse_mqtt_packet(const uint8_t *data, size_t size) {
    HParseResult *result = h_parse(mqtt_packet_parser(), data, size);
    if (result == NULL) {
        return NULL;
    }

    MQTT_Packet *packet = malloc(sizeof(MQTT_Packet));
    packet->packet_type = *(uint8_t *)result->ast->seq->elements[0]->token;
    packet->flags = *(uint8_t *)result->ast->seq->elements[1]->token;
    packet->packet_id = *(uint16_t *)result->ast->seq->elements[2]->token;
    packet->properties_length = *(uint32_t *)result->ast->seq->elements[3]->token;
    packet->properties = (uint8_t *)result->ast->seq->elements[4]->token;
    packet->payload = (uint8_t *)result->ast->seq->elements[5]->token;

    h_parse_result_free(result);
    return packet;
}

void free_mqtt_packet(MQTT_Packet *packet) {
    free(packet->properties);
    free(packet->payload);
    free(packet);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
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

    uint8_t *data = malloc(file_size);
    fread(data, 1, file_size, file);
    fclose(file);

    MQTT_Packet *packet = parse_mqtt_packet(data, file_size);
    if (!packet) {
        fprintf(stderr, "Failed to parse MQTT packet\n");
        free(data);
        return 1;
    }

    printf("Packet Type: %d\n", packet->packet_type);
    printf("Flags: %d\n", packet->flags);
    printf("Packet ID: %d\n", packet->packet_id);
    printf("Properties Length: %d\n", packet->properties_length);
    printf("Properties: ");
    for (uint32_t i = 0; i < packet->properties_length; i++) {
        printf("%02x ", packet->properties[i]);
    }
    printf("\nPayload: ");
    for (size_t i = 0; i < file_size - (4 + packet->properties_length); i++) {
        printf("%02x ", packet->payload[i]);
    }
    printf("\n");

    free_mqtt_packet(packet);
    free(data);
    return 0;
}