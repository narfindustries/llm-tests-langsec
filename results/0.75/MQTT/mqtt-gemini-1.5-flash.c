#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef enum {
  MQTT_CONNECT = 1,
  MQTT_CONNACK = 2,
  MQTT_PUBLISH = 3,
  MQTT_PUBACK = 4,
  MQTT_PUBREC = 5,
  MQTT_PUBREL = 6,
  MQTT_PUBCOMP = 7,
  MQTT_SUBSCRIBE = 8,
  MQTT_SUBACK = 9,
  MQTT_UNSUBSCRIBE = 10,
  MQTT_UNSUBACK = 11,
  MQTT_PINGREQ = 12,
  MQTT_PINGRESP = 13,
  MQTT_DISCONNECT = 14,
  MQTT_AUTH = 15 
} MqttPacketType;

static uint32_t read_variable_length_integer(hammer_parser_t *parser) {
    uint32_t value = 0;
    uint8_t byte;
    int multiplier = 1;

    do {
        if (hammer_parse_uint8(parser, &byte) != HAMMER_OK) return 0; 
        value += (byte & 0x7F) * multiplier;
        multiplier *= 128;
    } while (byte & 0x80);

    return value;
}

static char* read_utf8_string(hammer_parser_t *parser) {
    uint32_t len = read_variable_length_integer(parser);
    if (len > 1024 * 1024) return NULL; 
    char *str = (char*)malloc(len + 1);
    if (hammer_parse_bytes(parser, (uint8_t*)str, len) != HAMMER_OK) {
        free(str);
        return NULL; 
    }
    str[len] = '\0';
    return str;
}

hammer_result_t parse_mqtt_connect(hammer_parser_t* parser,  mqtt_connect_packet_t* packet) {
    uint8_t remaining_length;
    uint8_t protocol_name_len;
    char* protocol_name;
    uint8_t protocol_level;
    uint8_t connect_flags;
    uint32_t keep_alive;
    char* client_id;
    char* will_topic = NULL;
    char* will_message = NULL;
    uint8_t will_qos = 0;


    if (hammer_parse_uint8(parser, &remaining_length) != HAMMER_OK) return HAMMER_ERROR; 
    if (hammer_parse_uint8(parser, &protocol_name_len) != HAMMER_OK) return HAMMER_ERROR;
    protocol_name = (char*)malloc(protocol_name_len + 1);
    if (hammer_parse_bytes(parser, (uint8_t*)protocol_name, protocol_name_len) != HAMMER_OK) {
        free(protocol_name);
        return HAMMER_ERROR;
    }
    protocol_name[protocol_name_len] = '\0';
    if (hammer_parse_uint8(parser, &protocol_level) != HAMMER_OK) return HAMMER_ERROR;
    if (hammer_parse_uint8(parser, &connect_flags) != HAMMER_OK) return HAMMER_ERROR;
    keep_alive = read_variable_length_integer(parser);
    client_id = read_utf8_string(parser);

    if (connect_flags & 0x4) { 
        will_topic = read_utf8_string(parser);
        will_message = read_utf8_string(parser);
        will_qos = (connect_flags >> 3) & 0x3;
    }

    packet->protocol_name = protocol_name;
    packet->protocol_level = protocol_level;
    packet->connect_flags = connect_flags;
    packet->keep_alive = keep_alive;
    packet->client_id = client_id;
    packet->will_topic = will_topic;
    packet->will_message = will_message;
    packet->will_qos = will_qos;

    return HAMMER_OK;
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
    if(buffer == NULL){
        perror("Memory allocation failed");
        return 1;
    }
    fread(buffer, 1, fsize, fp);
    fclose(fp);

    hammer_parser_t *parser = hammer_parser_create_from_buffer(buffer, fsize);
    if (parser == NULL){
        fprintf(stderr,"Error creating parser\n");
        free(buffer);
        return 1;
    }

    mqtt_connect_packet_t connect_packet = {0}; 
    hammer_result_t result = parse_mqtt_connect(parser, &connect_packet);
    if(result != HAMMER_OK){
        fprintf(stderr, "Error parsing MQTT CONNECT packet\n");
    } else {
        printf("MQTT CONNECT Packet parsed successfully!\n");
        free(connect_packet.protocol_name);
        free(connect_packet.client_id);
        free(connect_packet.will_topic);
        free(connect_packet.will_message);
    }


    hammer_parser_destroy(parser);
    free(buffer);

    return 0;
}

typedef struct {
    char* protocol_name;
    uint8_t protocol_level;
    uint8_t connect_flags;
    uint32_t keep_alive;
    char* client_id;
    char* will_topic;
    char* will_message;
    uint8_t will_qos;
} mqtt_connect_packet_t;