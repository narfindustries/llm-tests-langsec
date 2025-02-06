#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#define MQTT_PROTOCOL_NAME "MQTT"
#define MQTT_PROTOCOL_LEVEL 5

typedef enum {
    MQTT_CONNECT = 0x01,
    MQTT_CONNACK = 0x02,
    MQTT_PUBLISH = 0x03,
    MQTT_PUBACK = 0x04,
    MQTT_PUBREC = 0x05,
    MQTT_PUBREL = 0x06,
    MQTT_PUBCOMP = 0x07,
    MQTT_SUBSCRIBE = 0x08,
    MQTT_SUBACK = 0x09,
    MQTT_UNSUBSCRIBE = 0x0A,
    MQTT_UNSUBACK = 0x0B,
    MQTT_PINGREQ = 0x0C,
    MQTT_PINGRESP = 0x0D,
    MQTT_DISCONNECT = 0x0E,
    MQTT_AUTH = 0x0F
} mqtt_control_packet_type;

typedef enum {
    MQTT_QOS_AT_MOST_ONCE = 0,
    MQTT_QOS_AT_LEAST_ONCE = 1,
    MQTT_QOS_EXACTLY_ONCE = 2
} mqtt_qos;

typedef struct {
    uint8_t type;
    uint8_t flags;
    uint32_t remaining_length;
} mqtt_fixed_header;

typedef struct {
    mqtt_fixed_header fixed_header;
    char* protocol_name;
    uint8_t protocol_level;
    uint8_t connect_flags;
    uint16_t keep_alive;
    uint32_t property_length;
    uint8_t* properties;
} mqtt_connect;

typedef struct {
    mqtt_fixed_header fixed_header;
    uint8_t session_present;
    uint8_t return_code;
    uint32_t property_length;
    uint8_t* properties;
} mqtt_connack;

typedef struct {
    mqtt_fixed_header fixed_header;
    char* topic_name;
    uint16_t packet_identifier;
    uint32_t property_length;
    uint8_t* properties;
    uint8_t* payload;
} mqtt_publish;

typedef struct {
    mqtt_fixed_header fixed_header;
    uint16_t packet_identifier;
    uint8_t reason_code;
    uint32_t property_length;
    uint8_t* properties;
} mqtt_puback;

typedef struct {
    mqtt_fixed_header fixed_header;
    uint16_t packet_identifier;
    uint8_t reason_code;
    uint32_t property_length;
    uint8_t* properties;
} mqtt_pubrec;

typedef struct {
    mqtt_fixed_header fixed_header;
    uint16_t packet_identifier;
    uint8_t reason_code;
    uint32_t property_length;
    uint8_t* properties;
} mqtt_pubrel;

typedef struct {
    mqtt_fixed_header fixed_header;
    uint16_t packet_identifier;
    uint8_t reason_code;
    uint32_t property_length;
    uint8_t* properties;
} mqtt_pubcomp;

typedef struct {
    mqtt_fixed_header fixed_header;
    uint16_t packet_identifier;
    uint32_t property_length;
    uint8_t* properties;
} mqtt_subscribe;

typedef struct {
    mqtt_fixed_header fixed_header;
    uint16_t packet_identifier;
    uint8_t reason_code;
    uint32_t property_length;
    uint8_t* properties;
} mqtt_suback;

typedef struct {
    mqtt_fixed_header fixed_header;
    uint16_t packet_identifier;
    uint32_t property_length;
    uint8_t* properties;
} mqtt_unsubscribe;

typedef struct {
    mqtt_fixed_header fixed_header;
    uint16_t packet_identifier;
    uint8_t reason_code;
    uint32_t property_length;
    uint8_t* properties;
} mqtt_unsuback;

typedef struct {
    mqtt_fixed_header fixed_header;
    uint8_t reason_code;
    uint32_t property_length;
    uint8_t* properties;
} mqtt_disconnect;

int main(int argc, char* argv[]) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file: %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* data = malloc(file_size);
    fread(data, 1, file_size, file);
    fclose(file);

    HParser* parser = h_new_parser(NULL);
    uint8_t* stream = data;
    size_t length = file_size;
    HParseResult* result = h_parse(parser, stream, length);
    if (result->status == HPARSE_ERROR) {
        printf("Error parsing packet: %s\n", h_get_parse_error_message(result));
        return 1;
    }

    printf("Parsed packet:\n");
    h_put_value(result->value, 0);

    h_free_parse_result(result);
    h_free_parser(parser);
    free(data);

    return 0;
}