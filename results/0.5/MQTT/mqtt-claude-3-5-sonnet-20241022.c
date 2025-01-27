#include "hammer.h"
#include <stdio.h>

static const HParser* mqtt_protocol_name = h_sequence(
    h_int_range(h_uint8(), 0, 6), // Length
    h_token((const uint8_t*)"MQIsdp", 6), // Protocol name
    NULL);

static const HParser* mqtt_connect_flags = h_bits(8, false);

static const HParser* mqtt_connect = h_sequence(
    mqtt_protocol_name,
    h_uint8(), // Protocol version
    mqtt_connect_flags,
    h_uint16(), // Keep alive
    h_length_value(h_uint16(), h_uint8()), // Client ID
    NULL);

static const HParser* mqtt_header = h_sequence(
    h_bits(4, false), // Message type
    h_bits(4, false), // Flags
    h_uint8(), // Remaining length
    NULL);

static const HParser* mqtt_publish = h_sequence(
    h_length_value(h_uint16(), h_uint8()), // Topic
    h_optional(h_uint16()), // Message ID (if QoS > 0)
    h_many(h_uint8()), // Payload
    NULL);

static const HParser* mqtt_subscribe = h_sequence(
    h_uint16(), // Message ID
    h_many1(h_sequence(
        h_length_value(h_uint16(), h_uint8()), // Topic filter
        h_uint8(), // QoS
        NULL)),
    NULL);

static const HParser* mqtt_unsubscribe = h_sequence(
    h_uint16(), // Message ID
    h_many1(h_length_value(h_uint16(), h_uint8())), // Topic filters
    NULL);

static const HParser* mqtt_packet = h_sequence(
    mqtt_header,
    h_choice(mqtt_connect,
             mqtt_publish,
             mqtt_subscribe,
             mqtt_unsubscribe,
             NULL),
    NULL);

H_RULE(mqtt_parser, mqtt_packet);

HParser* init_mqtt_parser(void) {
    return mqtt_parser;
}