#include <hammer/hammer.h>
#include <hammer/glue.h>

// Forward declarations for recursive rules
static HParsedToken *parse_payload(HRParser *, const HParseResult *, void *);

// MQTT Control Packet Types
static HParser *mqtt_connect;
static HParser *mqtt_connack;
static HParser *mqtt_publish;
static HParser *mqtt_puback;
static HParser *mqtt_pubrec;
static HParser *mqtt_pubrel;
static HParser *mqtt_pubcomp;
static HParser *mqtt_subscribe;
static HParser *mqtt_suback;
static HParser *mqtt_unsubscribe;
static HParser *mqtt_unsuback;
static HParser *mqtt_pingreq;
static HParser *mqtt_pingresp;
static HParser *mqtt_disconnect;

// Helper parsers
static HParser *fixed_header;
static HParser *variable_header;
static HParser *payload;

// Fixed Header
static HParser *packet_type;
static HParser *flags;
static HParser *remaining_length;

// Variable Header and Payload for specific packet types
static HParser *connect_flags;
static HParser *keep_alive;
static HParser *topic_name;
static HParser *message_id;
static HParser *topic_filter;
static HParser *qos;

// MQTT packet type definitions
static void init_mqtt_parsers() {
    packet_type = h_bits(4, false);
    flags = h_bits(4, false);
    remaining_length = h_uint8();  // Simplified for example

    fixed_header = h_sequence(packet_type, flags, remaining_length, NULL);

    // CONNECT
    connect_flags = h_uint8();
    keep_alive = h_uint16();
    variable_header = h_sequence(connect_flags, keep_alive, NULL);
    payload = h_action(h_arbitrary(), parse_payload, NULL);
    mqtt_connect = h_sequence(fixed_header, variable_header, payload, NULL);

    // CONNACK
    mqtt_connack = h_sequence(fixed_header, h_uint8(), NULL);  // Simplified

    // PUBLISH
    topic_name = h_length_value(h_uint16(), h_arbitrary_bytes());
    message_id = h_uint16();
    variable_header = h_sequence(topic_name, message_id, NULL);
    mqtt_publish = h_sequence(fixed_header, variable_header, payload, NULL);

    // PUBACK, PUBREC, PUBREL, PUBCOMP
    mqtt_puback = mqtt_pubrec = mqtt_pubrel = mqtt_pubcomp = h_sequence(fixed_header, message_id, NULL);

    // SUBSCRIBE
    message_id = h_uint16();
    topic_filter = h_length_value(h_uint16(), h_arbitrary_bytes());
    qos = h_bits(2, false);
    variable_header = h_sequence(message_id, NULL);
    payload = h_many1(h_sequence(topic_filter, qos, NULL));
    mqtt_subscribe = h_sequence(fixed_header, variable_header, payload, NULL);

    // SUBACK
    mqtt_suback = h_sequence(fixed_header, message_id, payload, NULL);

    // UNSUBSCRIBE
    mqtt_unsubscribe = h_sequence(fixed_header, variable_header, payload, NULL);

    // UNSUBACK
    mqtt_unsuback = h_sequence(fixed_header, message_id, NULL);

    // PINGREQ, PINGRESP, DISCONNECT
    mqtt_pingreq = mqtt_pingresp = mqtt_disconnect = fixed_header;
}

static HParsedToken *parse_payload(HRParser *p, const HParseResult *res, void *user_data) {
    // Custom payload parsing logic can be added here
    return h_make_bytes(res->bit_length, res->bit_offset, res->byte_length, res->byte_offset, res->seq);
}

int main(int argc, char **argv) {
    init_mqtt_parsers();

    // Example usage of mqtt_connect parser
    const uint8_t data[] = {0x10, 0x04, 0x00, 0x04, 'M', 'Q', 'T', 'T', 0x04, 0x02, 0x00, 0x3C, 0x00};
    size_t len = sizeof(data);
    HParseResult *result = h_parse(mqtt_connect, data, len);
    if (result) {
        printf("Parsed MQTT CONNECT packet successfully.\n");
        h_pprint(stdout, result->ast, 0, 0);
        h_parse_result_free(result);
    } else {
        printf("Failed to parse MQTT CONNECT packet.\n");
    }

    return 0;
}