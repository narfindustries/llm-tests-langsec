#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Define parsers for each MQTT packet type and fields
HParser *mqtt_string = h_bind_bytes(h_uint16(), h_uint16());
HParser *mqtt_binary = h_bind_bytes(h_uint16(), h_uint16());
HParser *mqtt_varint = h_repeat_n(h_uint8(), 1, 4);
HParser *mqtt_properties = h_many(h_choice(mqtt_varint, mqtt_string, mqtt_binary, NULL));

HParser *mqtt_connect_flags = h_bits(8, false);
HParser *mqtt_connect = h_sequence(
    h_token("MQTT", 4),
    h_uint8(), // Protocol Level
    mqtt_connect_flags,
    h_uint16(), // Keep Alive
    mqtt_properties,
    mqtt_string, // Client Identifier
    h_optional(h_sequence(mqtt_properties, mqtt_string, mqtt_binary, NULL)), // Will Properties, Will Topic, Will Payload
    h_optional(mqtt_string), // Username
    h_optional(mqtt_binary), // Password
    NULL
);

HParser *mqtt_connack_flags = h_bits(8, false);
HParser *mqtt_connack = h_sequence(
    mqtt_connack_flags,
    h_uint8(), // Reason Code
    mqtt_properties,
    NULL
);

HParser *mqtt_publish = h_sequence(
    h_bits(8, false), // DUP, QoS, Retain
    mqtt_string, // Topic Name
    h_optional(h_uint16()), // Packet Identifier
    mqtt_properties,
    h_left(h_uint32(), h_end_p()), // Payload
    NULL
);

HParser *mqtt_puback = h_sequence(
    h_uint16(), // Packet Identifier
    h_uint8(), // Reason Code
    mqtt_properties,
    NULL
);

HParser *mqtt_pubrec = mqtt_puback;
HParser *mqtt_pubrel = mqtt_puback;
HParser *mqtt_pubcomp = mqtt_puback;

HParser *mqtt_subscribe = h_sequence(
    h_uint16(), // Packet Identifier
    mqtt_properties,
    h_many1(h_sequence(mqtt_string, h_bits(8, false), NULL)), // Topic Filters and Options
    NULL
);

HParser *mqtt_suback = h_sequence(
    h_uint16(), // Packet Identifier
    mqtt_properties,
    h_many1(h_uint8()), // Reason Codes
    NULL
);

HParser *mqtt_unsubscribe = h_sequence(
    h_uint16(), // Packet Identifier
    mqtt_properties,
    h_many1(mqtt_string), // Topic Filters
    NULL
);

HParser *mqtt_unsuback = mqtt_suback;

HParser *mqtt_pingreq = h_end_p();
HParser *mqtt_pingresp = h_end_p();

HParser *mqtt_disconnect = h_sequence(
    h_uint8(), // Reason Code
    mqtt_properties,
    NULL
);

HParser *mqtt_auth = h_sequence(
    h_uint8(), // Reason Code
    mqtt_properties,
    NULL
);

// Main MQTT packet parser
HParser *mqtt_packet = h_choice(
    h_sequence(h_bits(4, false), mqtt_connect, NULL),
    h_sequence(h_bits(4, false), mqtt_connack, NULL),
    h_sequence(h_bits(4, false), mqtt_publish, NULL),
    h_sequence(h_bits(4, false), mqtt_puback, NULL),
    h_sequence(h_bits(4, false), mqtt_pubrec, NULL),
    h_sequence(h_bits(4, false), mqtt_pubrel, NULL),
    h_sequence(h_bits(4, false), mqtt_pubcomp, NULL),
    h_sequence(h_bits(4, false), mqtt_subscribe, NULL),
    h_sequence(h_bits(4, false), mqtt_suback, NULL),
    h_sequence(h_bits(4, false), mqtt_unsubscribe, NULL),
    h_sequence(h_bits(4, false), mqtt_unsuback, NULL),
    h_sequence(h_bits(4, false), mqtt_pingreq, NULL),
    h_sequence(h_bits(4, false), mqtt_pingresp, NULL),
    h_sequence(h_bits(4, false), mqtt_disconnect, NULL),
    h_sequence(h_bits(4, false), mqtt_auth, NULL),
    NULL
);

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(mqtt_packet, data, file_size);
    if (result) {
        printf("MQTT packet parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse MQTT packet.\n");
    }

    free(data);
    return EXIT_SUCCESS;
}