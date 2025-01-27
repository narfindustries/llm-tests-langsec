#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <string.h>

// MQTT Packet Type Definitions
typedef enum {
    CONNECT = 1,
    CONNACK = 2,
    PUBLISH = 3,
    PUBACK = 4,
    PUBREC = 5,
    PUBREL = 6,
    PUBCOMP = 7,
    SUBSCRIBE = 8,
    SUBACK = 9,
    UNSUBSCRIBE = 10,
    UNSUBACK = 11,
    PINGREQ = 12,
    PINGRESP = 13,
    DISCONNECT = 14
} MQTTPacketType;

// MQTT Fixed Header Parser
static HParsedToken* parse_fixed_header(void* p) {
    HParser* packet_type = h_bits_uint(4, false);
    HParser* flags = h_bits_uint(4, false);
    HParser* remaining_length = h_many1(h_ch_range(0, 127));
    
    HParser* fixed_header = h_sequence(packet_type, flags, remaining_length, NULL);
    return h_parse(fixed_header, p, NULL);
}

// MQTT Connect Packet Parser
static HParsedToken* parse_connect_packet(void* p) {
    HParser* protocol_name = h_length_value(h_bits_uint(16, false), h_ch_range(0, 255));
    HParser* protocol_level = h_ch_range(4, 4);  // MQTT 3.1.1
    
    HParser* connect_flags = h_bits_uint(8, false);
    HParser* keep_alive = h_bits_uint(16, false);
    
    HParser* client_id = h_length_value(h_bits_uint(16, false), h_ch_range(0, 255));
    
    HParser* connect_packet = h_sequence(
        protocol_name, 
        protocol_level, 
        connect_flags, 
        keep_alive, 
        client_id, 
        NULL
    );
    
    return h_parse(connect_packet, p, NULL);
}

// MQTT Publish Packet Parser
static HParsedToken* parse_publish_packet(void* p) {
    HParser* topic_name = h_length_value(h_bits_uint(16, false), h_ch_range(0, 255));
    HParser* message_payload = h_many1(h_ch_range(0, 255));
    
    HParser* publish_packet = h_sequence(topic_name, message_payload, NULL);
    
    return h_parse(publish_packet, p, NULL);
}

// Main MQTT Packet Parser
static HParsedToken* parse_mqtt_packet(void* p) {
    HParser* packet_type_parser = h_choice(
        parse_connect_packet,
        parse_publish_packet,
        NULL
    );
    
    return h_parse(packet_type_parser, p, NULL);
}

int main() {
    // Initialize Hammer
    h_init();
    
    // Create MQTT packet parser
    HParser* mqtt_parser = h_indirect();
    h_bind(mqtt_parser, parse_mqtt_packet);
    
    return 0;
}