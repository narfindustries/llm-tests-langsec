#include "hammer.h"
#include <stdio.h>

static const HParser* mqtt_protocol_name(void) {
    return h_sequence(
        h_int_range(h_uint8(), 0, 255),
        h_int_range(h_uint8(), 0, 255),
        h_int_range(h_uint8(), 0, 255),
        h_int_range(h_uint8(), 0, 255),
        NULL);
}

static const HParser* mqtt_message_type(void) {
    return h_bits(4, false);
}

static const HParser* mqtt_dup_flag(void) {
    return h_bits(1, false);
}

static const HParser* mqtt_qos_level(void) {
    return h_bits(2, false);
}

static const HParser* mqtt_retain(void) {
    return h_bits(1, false);
}

static const HParser* mqtt_remaining_length(void) {
    return h_many1(h_int_range(h_uint8(), 0, 255));
}

static const HParser* mqtt_fixed_header(void) {
    return h_sequence(
        mqtt_message_type(),
        mqtt_dup_flag(),
        mqtt_qos_level(),
        mqtt_retain(),
        mqtt_remaining_length(),
        NULL);
}

static const HParser* mqtt_connect_flags(void) {
    return h_sequence(
        h_bits(1, false),  // username flag
        h_bits(1, false),  // password flag
        h_bits(1, false),  // will retain
        h_bits(2, false),  // will QoS
        h_bits(1, false),  // will flag
        h_bits(1, false),  // clean session
        h_bits(1, false),  // reserved
        NULL);
}

static const HParser* mqtt_connect_packet(void) {
    return h_sequence(
        mqtt_fixed_header(),
        mqtt_protocol_name(),
        h_int_range(h_uint8(), 0, 255),  // protocol version
        mqtt_connect_flags(),
        h_int_range(h_uint16(), 0, 65535),  // keep alive
        NULL);
}

static const HParser* mqtt_parser(void) {
    return mqtt_connect_packet();
}

const HParser* init_parser(void) {
    return mqtt_parser();
}