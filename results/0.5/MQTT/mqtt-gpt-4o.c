#include <hammer/hammer.h>

HParser *mqtt_fixed_header_parser() {
    HParser *type = h_bits(4, false);
    HParser *flags = h_bits(4, false);
    HParser *remaining_length = h_length_encoded(h_bits(7, false), h_bits(1, false), h_uint8());

    return h_sequence(type, flags, remaining_length, NULL);
}

HParser *mqtt_connect_parser() {
    HParser *protocol_name = h_sequence(h_uint8(), h_uint8(), h_ch('M'), h_ch('Q'), h_ch('T'), h_ch('T'), NULL);
    HParser *protocol_level = h_uint8();
    HParser *connect_flags = h_bits(8, false);
    HParser *keep_alive = h_uint16();

    return h_sequence(protocol_name, protocol_level, connect_flags, keep_alive, NULL);
}

HParser *mqtt_variable_header_parser() {
    return h_choice(mqtt_connect_parser(), NULL);
}

HParser *mqtt_payload_parser() {
    return h_many(h_uint8());
}

HParser *mqtt_packet_parser() {
    HParser *fixed_header = mqtt_fixed_header_parser();
    HParser *variable_header = mqtt_variable_header_parser();
    HParser *payload = mqtt_payload_parser();

    return h_sequence(fixed_header, variable_header, payload, NULL);
}

int main() {
    HParser *parser = mqtt_packet_parser();
    // Use the parser as needed, for example, to parse a buffer
    // uint8_t *buffer = ...;
    // HParseResult *result = h_parse(parser, buffer, buffer_length);

    h_parser_free(parser);
    return 0;
}