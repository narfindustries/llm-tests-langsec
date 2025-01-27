#include <hammer/hammer.h>
#include <hammer/parsers.h>

static HParser* mqtt_packet_parser(void) {
    // MQTT Packet Types
    HParsedToken* parse_connect_packet(void* payload) { 
        // Connect packet parsing logic
        return NULL;
    }

    HParsedToken* parse_publish_packet(void* payload) {
        // Publish packet parsing logic 
        return NULL;
    }

    HParsedToken* parse_subscribe_packet(void* payload) {
        // Subscribe packet parsing logic
        return NULL;
    }

    HParser* packet_type = h_choice(
        h_action(h_literal("\x10"), parse_connect_packet),    // CONNECT 
        h_action(h_literal("\x30"), parse_publish_packet),    // PUBLISH
        h_action(h_literal("\x80"), parse_subscribe_packet)   // SUBSCRIBE
    );

    HParser* remaining_length = h_many(h_in_range(0x00, 0x7F));
    HParser* packet = h_sequence(packet_type, remaining_length, NULL);

    return packet;
}

int main(int argc, char** argv) {
    HParser* parser = mqtt_packet_parser();
    h_compile(parser, "mqtt_spec");
    return 0;
}