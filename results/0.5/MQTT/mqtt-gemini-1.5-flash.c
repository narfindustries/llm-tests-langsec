#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <hammer/hammer.h>

typedef struct {
    uint8_t type;
    uint8_t flags;
    uint16_t packet_id;
    char* topic;
    uint8_t* payload;
    size_t payload_len;
} mqtt_message_t;

hm_parser_t* parse_string(void){
    return hm_string();
}

hm_parser_t* parse_uint8(void){
    return hm_uint8();
}

hm_parser_t* parse_uint16(void){
    return hm_uint16();
}

hm_parser_t* parse_bytes(void){
    return hm_bytes();
}

hm_parser_t* parse_mqtt_message(void) {
    return hm_seq(
        parse_uint8(), 
        parse_uint8(), 
        parse_uint16(), 
        parse_string(), 
        parse_bytes(), 
        hm_end(), 
        NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
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

    uint8_t *buffer = (uint8_t *)malloc(fsize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }

    fread(buffer, 1, fsize, fp);
    fclose(fp);

    hm_parser_t* parser = parse_mqtt_message();
    hm_result_t result = hm_parse(parser, buffer, fsize);

    if (result.success) {
        mqtt_message_t* message = (mqtt_message_t*)result.value;
        printf("MQTT Message Parsed Successfully:\n");
        printf("Type: %u\n", message->type);
        printf("Flags: %u\n", message->flags);
        printf("Packet ID: %u\n", message->packet_id);
        printf("Topic: %s\n", message->topic);
        printf("Payload Length: %zu\n", message->payload_len);
        free(message->topic);
        free(message->payload);
        free(message);
    } else {
        fprintf(stderr, "MQTT Message Parsing Failed: %s\n", result.error);
    }

    hm_free(parser);
    free(buffer);
    return 0;
}
