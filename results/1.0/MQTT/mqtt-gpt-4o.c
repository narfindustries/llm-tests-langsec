#include <hammer/hammer.h>

HParser *mqtt_packet();

HParser *mqtt_fixed_header() {
    return h_sequence(
        h_bits(4, false),   // MQTT Control Packet type
        h_bits(4, false),   // Flags
        h_length(
            h_bits(8, false),  // Remaining length
            h_repeat(h_bits(8, false)) // Continue while MSB is 1
        ),
        NULL
    );
}

HParser *mqtt_variable_header() {
    // Variable header parsing logic goes here
    return h_nil();
}

HParser *mqtt_payload() {
    // Payload parsing logic goes here
    return h_nil();
}

HParser *mqtt_packet() {
    return h_sequence(
        mqtt_fixed_header(),
        mqtt_variable_header(),
        mqtt_payload(),
        NULL
    );
}

int main(int argc, char *argv[]) {
    HParser *parser = mqtt_packet();
    if (!parser) {
        fprintf(stderr, "Failed to create MQTT parser\n");
        return 1;
    }

    // Example usage of the created parser
    HParsedToken *result = h_parse(parser, (const uint8_t*)argv[1], strlen(argv[1]));
    if (result) {
        printf("Parse succeeded\n");
        h_token_free(result);
    } else {
        printf("Parse failed\n");
    }

    h_parser_free(parser);
    return 0;
}