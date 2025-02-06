#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *mqtt_string() {
    return h_sequence(h_uint16(), h_data(h_length_value(h_uint16(), h_uint8())), NULL);
}

HParser *mqtt_binary_data() {
    return h_sequence(h_uint16(), h_data(h_length_value(h_uint16(), h_uint8())), NULL);
}

HParser *mqtt_properties() {
    return h_many(h_choice(h_uint8(), h_uint16(), h_uint32(), h_uint64(), NULL));
}

HParser *mqtt_connect() {
    return h_sequence(
        mqtt_string(), // Protocol Name
        h_uint8(),     // Protocol Level
        h_uint8(),     // Connect Flags
        h_uint16(),    // Keep Alive
        mqtt_properties(),
        mqtt_string(), // Client Identifier
        h_optional(h_sequence(mqtt_properties(), mqtt_string(), mqtt_binary_data(), NULL)), // Will
        h_optional(mqtt_string()), // Username
        h_optional(mqtt_binary_data()), // Password
        NULL
    );
}

HParser *mqtt_connack() {
    return h_sequence(
        h_uint8(), // Session Present
        h_uint8(), // Connect Reason Code
        mqtt_properties(),
        NULL
    );
}

HParser *mqtt_publish() {
    return h_sequence(
        h_uint8(), // DUP, QoS, Retain
        mqtt_string(), // Topic Name
        h_optional(h_uint16()), // Packet Identifier
        mqtt_properties(),
        h_data(h_length_value(h_uint16(), h_uint8())), // Payload
        NULL
    );
}

HParser *mqtt_puback() {
    return h_sequence(
        h_uint16(), // Packet Identifier
        h_uint8(),  // Reason Code
        mqtt_properties(),
        NULL
    );
}

HParser *mqtt_pubrec() {
    return mqtt_puback();
}

HParser *mqtt_pubrel() {
    return mqtt_puback();
}

HParser *mqtt_pubcomp() {
    return mqtt_puback();
}

HParser *mqtt_subscribe() {
    return h_sequence(
        h_uint16(), // Packet Identifier
        mqtt_properties(),
        h_many(h_sequence(mqtt_string(), h_uint8(), NULL)), // Topic Filters and Options
        NULL
    );
}

HParser *mqtt_suback() {
    return h_sequence(
        h_uint16(), // Packet Identifier
        mqtt_properties(),
        h_many(h_uint8()), // Reason Codes
        NULL
    );
}

HParser *mqtt_unsubscribe() {
    return h_sequence(
        h_uint16(), // Packet Identifier
        mqtt_properties(),
        h_many(mqtt_string()), // Topic Filters
        NULL
    );
}

HParser *mqtt_unsuback() {
    return mqtt_suback();
}

HParser *mqtt_pingreq() {
    return h_sequence(NULL);
}

HParser *mqtt_pingresp() {
    return h_sequence(NULL);
}

HParser *mqtt_disconnect() {
    return h_sequence(
        h_uint8(), // Reason Code
        mqtt_properties(),
        NULL
    );
}

HParser *mqtt_auth() {
    return h_sequence(
        h_uint8(), // Reason Code
        mqtt_properties(),
        NULL
    );
}

HParser *mqtt_packet() {
    return h_choice(
        mqtt_connect(),
        mqtt_connack(),
        mqtt_publish(),
        mqtt_puback(),
        mqtt_pubrec(),
        mqtt_pubrel(),
        mqtt_pubcomp(),
        mqtt_subscribe(),
        mqtt_suback(),
        mqtt_unsubscribe(),
        mqtt_unsuback(),
        mqtt_pingreq(),
        mqtt_pingresp(),
        mqtt_disconnect(),
        mqtt_auth(),
        NULL
    );
}

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

    HParser *parser = mqtt_packet();
    HParseResult *result = h_parse(parser, data, file_size);

    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed.\n");
    }

    free(data);
    return EXIT_SUCCESS;
}