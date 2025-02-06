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
} MqttPacketType;

typedef enum {
  MQTT_PROP_SESSION_EXPIRY_INTERVAL = 1,
  MQTT_PROP_RECEIVE_MAXIMUM = 2,
} MqttPropertyId;

static uint32_t read_variable_length_integer(const uint8_t **data) {
    uint32_t value = 0;
    uint8_t digit;
    int multiplier = 1;
    do {
        digit = **data;
        (*data)++;
        value += (digit & 0x7F) * multiplier;
        multiplier *= 128;
    } while ((digit & 0x80) != 0);
    return value;
}

static char* read_utf8_string(const uint8_t **data, uint32_t length) {
    char* str = (char*)malloc(length + 1);
    memcpy(str, *data, length);
    str[length] = '\0';
    *data += length;
    return str;
}

static HParser mqtt_property_parser() {
  return hseq(h_uint16(), h_bytes(0)); // Placeholder - needs full property spec
}

static HParser mqtt_connect_parser() {
    return h_seq(
        h_const(MQTT_CONNECT), 
        h_map(read_variable_length_integer, h_uint16()), 
        h_string(), 
        h_const(5), 
        h_uint8(), 
        h_uint16(), 
        h_map(read_utf8_string, h_uint16()), 
        h_opt(h_map(read_utf8_string, h_uint16())), 
        h_opt(h_map(read_utf8_string, h_uint16())), 
        h_opt(h_map(read_utf8_string, h_uint16())), 
        h_opt(h_map(read_utf8_string, h_uint16())), 
        h_many(mqtt_property_parser()), 
        h_bytes(0) 
    );
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
    fread(buffer, 1, fsize, fp);
    fclose(fp);

    const uint8_t *data = buffer;
    HResult result = h_parse(mqtt_connect_parser(), &data, buffer + fsize);

    if (result.success) {
        printf("MQTT CONNECT packet parsed successfully!\n");
    } else {
        fprintf(stderr, "MQTT packet parsing failed at offset %lu: %s\n", (long)(data - buffer), result.error);
    }

    free(buffer);
    return 0;
}
