#include <hammer/hammer.h>

const HParser* mqtt_parser() {
    // Basic integer parsers
    HParser* uint8 = h_uint8();
    HParser* uint16 = h_uint16();

    // Fixed header
    HParser* packet_type = h_bits(4, false);
    HParser* flags = h_bits(4, false);
    HParser* remaining_length = h_length_value(h_uint8(), h_uint8());
    HParser* fixed_header = h_sequence(packet_type, flags, remaining_length, NULL);

    // Variable header components
    HParser* protocol_name_length = h_uint16();
    HParser* protocol_name = h_length_value(protocol_name_length, h_uint8());
    HParser* protocol_version = h_uint8();
    HParser* connect_flags = h_uint8();
    HParser* keep_alive = h_uint16();

    // Payload components
    HParser* string_length = h_uint16();
    HParser* string = h_length_value(string_length, h_uint8());
    HParser* client_id = string;
    HParser* username = h_optional(string);
    HParser* password = h_optional(string);
    HParser* will_topic = h_optional(string);
    HParser* will_message = h_optional(string);

    // Variable header
    HParser* variable_header = h_sequence(protocol_name, protocol_version, 
                                        connect_flags, keep_alive, NULL);

    // Payload
    HParser* payload = h_sequence(client_id, will_topic, will_message, 
                                username, password, NULL);

    // Complete MQTT CONNECT packet
    return h_sequence(fixed_header, variable_header, payload, NULL);
}

H_RULE(mqtt_protocol) {
    return mqtt_parser();
}