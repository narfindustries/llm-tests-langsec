#include <hammer/hammer.h>
#include <hammer/glue.h>

static HParser *mqtt_control_packet;
static HParser *mqtt_string;
static HParser *mqtt_variable_byte_integer;
static HParser *mqtt_payload;
static HParser *mqtt_connect_packet;
static HParser *mqtt_connack_packet;
static HParser *mqtt_publish_packet;
static HParser *mqtt_subscribe_packet;
static HParser *mqtt_suback_packet;
static HParser *mqtt_unsubscribe_packet;
static HParser *mqtt_unsuback_packet;
static HParser *mqtt_pingreq_packet;
static HParser *mqtt_pingresp_packet;
static HParser *mqtt_disconnect_packet;

static void init_mqtt_parser() {
    H_RULE(uint8, h_uint8());
    H_RULE(uint16, h_uint16());

    mqtt_string = h_length_value(h_bits(16, false), h_uint8());

    mqtt_variable_byte_integer = h_length_value(h_bits(8, false), h_uint8());

    // MQTT Control Packet Types
    mqtt_connect_packet = h_sequence(
        mqtt_string, // Protocol Name
        h_uint8(),   // Protocol Level
        h_uint8(),   // Connect Flags
        h_uint16(),  // Keep Alive
        mqtt_payload, // Client Identifier, Will Topic, Will Message, Username, Password
        NULL);

    mqtt_connack_packet = h_sequence(
        h_uint8(), // Acknowledge Flags
        h_uint8(), // Return Code
        NULL);

    mqtt_publish_packet = h_sequence(
        mqtt_string, // Topic Name
        h_optional(h_uint16()), // Packet Identifier
        mqtt_payload, // Message
        NULL);

    mqtt_subscribe_packet = h_sequence(
        h_uint16(), // Packet Identifier
        h_many1(h_sequence(mqtt_string, h_uint8())), // Topic Filters and QoS
        NULL);

    mqtt_suback_packet = h_sequence(
        h_uint16(), // Packet Identifier
        h_many1(h_uint8()), // Return Codes
        NULL);

    mqtt_unsubscribe_packet = h_sequence(
        h_uint16(), // Packet Identifier
        h_many1(mqtt_string), // Topic Filters
        NULL);

    mqtt_unsuback_packet = h_sequence(
        h_uint16(), // Packet Identifier
        NULL);

    mqtt_pingreq_packet = h_empty();   // PINGREQ has no variable header or payload
    mqtt_pingresp_packet = h_empty();  // PINGRESP has no variable header or payload
    mqtt_disconnect_packet = h_empty(); // DISCONNECT has no variable header or payload

    mqtt_control_packet = h_choice(
        h_sequence(h_int_range(h_uint8(), 0x10, 0x10), mqtt_connect_packet),
        h_sequence(h_int_range(h_uint8(), 0x20, 0x20), mqtt_connack_packet),
        h_sequence(h_int_range(h_uint8(), 0x30, 0x30), mqtt_publish_packet),
        h_sequence(h_int_range(h_uint8(), 0x80, 0x80), mqtt_subscribe_packet),
        h_sequence(h_int_range(h_uint8(), 0x90, 0x90), mqtt_suback_packet),
        h_sequence(h_int_range(h_uint8(), 0xA0, 0xA0), mqtt_unsubscribe_packet),
        h_sequence(h_int_range(h_uint8(), 0xB0, 0xB0), mqtt_unsuback_packet),
        h_sequence(h_int_range(h_uint8(), 0xC0, 0xC0), mqtt_pingreq_packet),
        h_sequence(h_int_range(h_uint8(), 0xD0, 0xD0), mqtt_pingresp_packet),
        h_sequence(h_int_range(h_uint8(), 0xE0, 0xE0), mqtt_disconnect_packet),
        NULL);
}

int main(int argc, char *argv[]) {
    init_mqtt_parser();
    HParseResult *result = h_parse(mqtt_control_packet, (const uint8_t *)argv[1], strlen(argv[1]));
    if (result) {
        h_pprint(stdout, result->ast, 0, 0);
        h_parse_result_free(result);
        return 0;
    } else {
        fprintf(stderr, "Failed to parse MQTT data.\n");
        return 1;
    }
}