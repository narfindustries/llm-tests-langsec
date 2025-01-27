#include <hammer/hammer.h>
#include <stdio.h>

HParser *create_mqtt_parser() {
    // Define the basic components of the MQTT protocol
    HParser *byte = h_uint8();
    HParser *remaining_length = h_int8();  // Simplified for demo purposes

    // MQTT Control Packet format
    // Fixed Header: 2 bytes (Control Packet Type and Flags, Remaining Length)
    HParser *control_packet_type_and_flags = byte;
    HParser *fixed_header = h_sequence(control_packet_type_and_flags, remaining_length, NULL);

    // MQTT Connect Packet - Variable Header
    HParser *protocol_name = h_string("MQTT", 4);
    HParser *protocol_level = byte;
    HParser *connect_flags = byte;
    HParser *keep_alive = h_uint16();
    HParser *variable_header = h_sequence(protocol_name, protocol_level, connect_flags, keep_alive, NULL);

    // MQTT Connect Packet - Payload
    // For simplicity, assume payload is a single string
    HParser *client_id = h_string_length_prefixed(h_uint16(), byte);
    HParser *payload = client_id;

    // Complete MQTT Connect Packet
    HParser *mqtt_connect_packet = h_sequence(fixed_header, variable_header, payload, NULL);

    return mqtt_connect_packet;
}

int main() {
    HParser *mqtt_parser = create_mqtt_parser();
    if (!mqtt_parser) {
        fprintf(stderr, "Error creating MQTT parser\n");
        return 1;
    }

    // Example buffer (assuming a valid MQTT packet for testing)
    uint8_t buffer[] = {0x10, 0x0F, 'M', 'Q', 'T', 'T', 0x04, 0x02, 0x00, 0x3C, 0x00, 0x04, 't', 'e', 's', 't'};
    size_t buffer_len = sizeof(buffer);

    const uint8_t *cur = buffer;
    HParseResult *result = h_parse(mqtt_parser, buffer, buffer_len);
    if (result) {
        printf("Parsed MQTT packet successfully\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse MQTT packet\n");
    }

    h_parser_free(mqtt_parser);
    return 0;
}