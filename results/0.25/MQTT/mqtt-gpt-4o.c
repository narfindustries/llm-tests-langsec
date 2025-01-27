#include <hammer/hammer.h>

typedef struct {
    uint8_t length;
    char *value;
} mqtt_string;

static HParser *mqtt_string_parser(void) {
    return h_sequence(
        h_uint8(),
        h_length_value(h_uint8(), h_uint8()),
        NULL
    );
}

static HParser *mqtt_fixed_header_parser(void) {
    return h_sequence(
        h_bits(4, false), // MQTT Control Packet type
        h_bits(4, false), // Flags
        h_uint8(),        // Remaining Length
        NULL
    );
}

static HParser *mqtt_connect_flags_parser(void) {
    return h_bits(8, false); // Connect Flags
}

static HParser *mqtt_connect_parser(void) {
    return h_sequence(
        mqtt_fixed_header_parser(),
        h_uint16(), // Protocol Name Length
        h_string(4), // Protocol Name "MQTT"
        h_uint8(),   // Protocol Level
        mqtt_connect_flags_parser(),
        h_uint16(),  // Keep Alive
        mqtt_string_parser(), // Client Identifier
        NULL
    );
}

static HParser *mqtt_packet_parser(void) {
    return h_choice(
        mqtt_connect_parser(),
        NULL
    );
}

int main(int argc, char **argv) {
    HParser *parser = mqtt_packet_parser();
    // Use the parser with input data
    // Example: h_parse(parser, input_data, input_length);
    h_parser_free(parser);
    return 0;
}