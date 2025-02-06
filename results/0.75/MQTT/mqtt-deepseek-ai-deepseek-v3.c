#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define MQTT parsers
HParser *mqtt_connect_parser() {
    return h_sequence(
        h_uint8(), // Protocol Name length
        h_token("MQTT", 4), // Protocol Name
        h_uint8(), // Protocol Level
        h_uint8(), // Connect Flags
        h_uint16(), // Keep Alive
        h_uint8(), // Properties length
        h_many(h_sequence(h_uint8(), h_uint32())), // Properties
        NULL
    );
}

HParser *mqtt_connack_parser() {
    return h_sequence(
        h_uint8(), // Session Present
        h_uint8(), // Reason Code
        h_uint8(), // Properties length
        h_many(h_sequence(h_uint8(), h_uint32())), // Properties
        NULL
    );
}

HParser *mqtt_publish_parser() {
    return h_sequence(
        h_uint16(), // Topic Name length
        h_length_value(h_uint16(), h_bits(8, false)), // Topic Name
        h_uint16(), // Packet Identifier
        h_uint8(), // Properties length
        h_many(h_sequence(h_uint8(), h_uint32())), // Properties
        h_length_value(h_uint32(), h_bits(8, false)), // Payload
        NULL
    );
}

HParser *mqtt_subscribe_parser() {
    return h_sequence(
        h_uint16(), // Packet Identifier
        h_uint8(), // Properties length
        h_many(h_sequence(h_uint8(), h_uint32())), // Properties
        h_many(h_sequence(h_length_value(h_uint16(), h_bits(8, false)), h_uint8())), // Topic Filters and Options
        NULL
    );
}

HParser *mqtt_unsubscribe_parser() {
    return h_sequence(
        h_uint16(), // Packet Identifier
        h_uint8(), // Properties length
        h_many(h_sequence(h_uint8(), h_uint32())), // Properties
        h_many(h_length_value(h_uint16(), h_bits(8, false))), // Topic Filters
        NULL
    );
}

HParser *mqtt_disconnect_parser() {
    return h_sequence(
        h_uint8(), // Reason Code
        h_uint8(), // Properties length
        h_many(h_sequence(h_uint8(), h_uint32())), // Properties
        NULL
    );
}

HParser *mqtt_auth_parser() {
    return h_sequence(
        h_uint8(), // Reason Code
        h_uint8(), // Properties length
        h_many(h_sequence(h_uint8(), h_uint32())), // Properties
        NULL
    );
}

HParser *mqtt_packet_parser() {
    return h_choice(
        mqtt_connect_parser(),
        mqtt_connack_parser(),
        mqtt_publish_parser(),
        mqtt_subscribe_parser(),
        mqtt_unsubscribe_parser(),
        mqtt_disconnect_parser(),
        mqtt_auth_parser(),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Error allocating memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(mqtt_packet_parser(), buffer, file_size);
    if (!result) {
        fprintf(stderr, "Failed to parse MQTT packet\n");
        free(buffer);
        return 1;
    }

    // Use the parsed result here
    // Example: Print the parsed data
    printf("Parsed MQTT packet successfully\n");

    h_parse_result_free(result);
    free(buffer);
    return 0;
}