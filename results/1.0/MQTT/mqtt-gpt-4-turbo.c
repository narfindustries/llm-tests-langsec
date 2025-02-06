#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Utility function to create bytes parser
HParser *fixed_string(const char *s, size_t n) {
    return h_token(s, n);
}

HParser *var_int() {
    return h_uint32();
}

HParser *field_length() {
    return h_uint16();
}

HParser *protocol_name() {
    return fixed_string("\x00\x04MQTT", 6);
}

HParser *protocol_level() {
    return h_uint8();
}

// Connect Flags parser using correct bit parsing functions
HParser *connect_flags() {
    return h_bits(8, false, 
        h_bit(), // username flag
        h_bit(), // password flag
        h_bit(), // will retain
        h_bits(2, false), // will QoS
        h_bit(), // will flag
        h_bit(), // clean session
        h_bit()  // reserved
    );
}

HParser *keep_alive() {
    return h_uint16();
}

HParser *mqtt_string() {
    return h_length_value(field_length(), h_int8());
}

// Correct usage of h_many
HParser *connect_message() {
    return h_sequence(
        protocol_name(),
        protocol_level(),
        connect_flags(),
        keep_alive(),
        h_many1(mqtt_string()), // properties and client identifier
        NULL
    );
}

HParser *topic_name() {
    return mqtt_string();
}

HParser *publish_packet() {
    return h_sequence(
        topic_name(),
        h_optional(h_uint16()), // Packet identifier (optional based on QoS level)
        h_many1(h_int8()), // Payload
        NULL
    );
}

HParser *subscribe_topic() {
    return h_sequence(
        topic_name(),
        h_uint8(), // Options byte
        NULL
    );
}

HParser *subscribe_message() {
    return h_sequence(
        h_uint16(),
        h_many1(subscribe_topic()),
        NULL
    );
}

HParser *mqtt_packet() {
    return h_choice(
        connect_message(),
        publish_packet(),
        subscribe_message(),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return 1;
    }

    FILE *f = fopen(argv[1], "rb");
    if (f == NULL) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t sz = ftell(f);
    rewind(f);

    uint8_t *buffer = malloc(sz);
    if (buffer == NULL) {
        fprintf(stderr, "Failed to allocate memory\n");
        fclose(f);
        return 1;
    }

    fread(buffer, 1, sz, f);
    fclose(f);

    HParser *mqttParser = mqtt_packet();
    HParseResult *result = h_parse(mqttParser, buffer, sz);

    if (result) {
        printf("Parsing successful!\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        printf("Parsing failed!\n");
    }

    free(buffer);
    h_parse_result_free(result);
    h_parser_free(mqttParser);

    return 0;
}