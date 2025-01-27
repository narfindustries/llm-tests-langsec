#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// MQTT Control Packet Type Parser
static const HParser* mqtt_packet_type_parser() {
    return h_choice(
        h_literal_uint(1),  // CONNECT
        h_literal_uint(2),  // CONNACK
        h_literal_uint(3),  // PUBLISH
        h_literal_uint(4),  // PUBACK
        h_literal_uint(5),  // PUBREC
        h_literal_uint(6),  // PUBREL
        h_literal_uint(7),  // PUBCOMP
        h_literal_uint(8),  // SUBSCRIBE
        h_literal_uint(9),  // SUBACK
        h_literal_uint(10), // UNSUBSCRIBE
        h_literal_uint(11), // UNSUBACK
        h_literal_uint(12), // PINGREQ
        h_literal_uint(13), // PINGRESP
        h_literal_uint(14)  // DISCONNECT
    );
}

// Fixed Header Parser
static const HParser* mqtt_fixed_header_parser() {
    return h_sequence(
        mqtt_packet_type_parser(),
        h_bits(4, false),   // Flags
        h_length_value(h_varint())  // Remaining Length
    );
}

// Variable Header for CONNECT Packet
static const HParser* mqtt_connect_variable_header_parser() {
    return h_sequence(
        h_string_z("MQTT"),  // Protocol Name
        h_bits(8, false),    // Protocol Level
        h_bits(8, false),    // Connect Flags
        h_bits(16, false)    // Keep Alive
    );
}

// Main MQTT Packet Parser
static const HParser* mqtt_packet_parser() {
    return h_choice(
        mqtt_fixed_header_parser(),
        mqtt_connect_variable_header_parser()
    );
}

int main(int argc, char** argv) {
    HParseResult* result;
    const uint8_t* input;
    size_t input_len;

    // Initialize Hammer
    h_init();

    // Compile Parser
    const HParser* parser = mqtt_packet_parser();

    // Mock input for testing
    uint8_t test_input[] = {0x10, 0x0F, 0x00, 0x04, 'M', 'Q', 'T', 'T'};
    input = test_input;
    input_len = sizeof(test_input);

    // Parse Input
    result = h_parse(parser, input, input_len);

    if (result) {
        printf("Parsing successful\n");
    } else {
        printf("Parsing failed\n");
    }

    return 0;
}