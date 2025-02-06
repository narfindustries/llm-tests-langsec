#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef struct {
    uint8_t type;
    uint8_t flags;
    uint16_t packet_id;
    char* payload;
    size_t payload_len;
} mqtt_publish_packet;

hammer_parser mqtt_publish_parser() {
    hammer_parser payload_parser = hammer_map(hammer_take(hammer_uint32()), (void*(*)(size_t))malloc, free);

    return hammer_seq(
        hammer_uint8(),
        hammer_uint8(),
        hammer_uint16(),
        payload_parser,
        hammer_ignore(hammer_uint32())
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <mqtt_binary_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char *buffer = (char *)malloc(fsize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }

    fread(buffer, 1, fsize, fp);
    fclose(fp);

    hammer_parser parser = mqtt_publish_parser();
    hammer_result result = hammer_parse(parser, buffer, fsize);

    if (result.success) {
        mqtt_publish_packet *packet = (mqtt_publish_packet *)result.value;
        printf("MQTT Publish Packet:\n");
        printf("Type: %u\n", packet->type);
        printf("Flags: %u\n", packet->flags);
        printf("Packet ID: %u\n", packet->packet_id);
        printf("Payload Length: %zu\n", packet->payload_len);
        if (packet->payload) {
          printf("Payload: %.*s\n", (int)packet->payload_len, packet->payload);
          free(packet->payload);
        }
        free(packet);
    } else {
        fprintf(stderr, "Parsing failed at position %zu: %s\n", result.position, result.error);
    }

    free(buffer);
    return 0;
}
