#include <hammer/hammer.h>

HParser *mqtt_parser(void) {
    // Define MQTT fixed header
    HParser *mqtt_type = h_bits(4, false);
    HParser *mqtt_flags = h_bits(4, false);
    HParser *mqtt_remaining_length = h_varint();

    // Define MQTT variable header and payload (simplified for demonstration)
    HParser *mqtt_variable_header = h_sequence(
        h_uint8(), // Packet Identifier MSB
        h_uint8(), // Packet Identifier LSB
        NULL
    );

    HParser *mqtt_payload = h_many(h_uint8());

    // Define complete MQTT message parser
    HParser *mqtt_message = h_sequence(
        mqtt_type,
        mqtt_flags,
        mqtt_remaining_length,
        mqtt_variable_header,
        mqtt_payload,
        NULL
    );

    return mqtt_message;
}

int main(int argc, char **argv) {
    HParser *parser = mqtt_parser();
    HParseResult *result;
    const uint8_t data[] = {0x30, 0x0A, 0x00, 0x04, 0x6D, 0x71, 0x74, 0x74, 0x04, 0x00, 0x00, 0x00};
    size_t data_len = sizeof(data) / sizeof(data[0]);

    result = h_parse(parser, data, data_len);

    if (result) {
        printf("Parsing succeeded.\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed.\n");
    }

    h_parser_free(parser);
    return 0;
}