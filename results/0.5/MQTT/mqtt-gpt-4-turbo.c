#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Define parser for MQTT Control Packet Types
static HParser *mqtt_control_packet_type() {
    return h_bits(4, false);
}

// Define parser for Flags based on packet type
static HParser *mqtt_flags(uint8_t packet_type) {
    switch(packet_type) {
        case 1: // CONNECT
        case 2: // CONNACK
            return h_bits(4, false);
        case 3: // PUBLISH
            return h_bits(4, true); // Flags are important for PUBLISH
        default:
            return h_ch(0); // Other types have reserved flags
    }
}

// Define parser for Variable Header based on packet type
static HParser *mqtt_variable_header(uint8_t packet_type) {
    switch(packet_type) {
        case 1: // CONNECT
            return h_sequence(h_string_s("MQTT"), h_uint8(), h_uint8(), h_uint16(), NULL);
        case 2: // CONNACK
            return h_sequence(h_uint8(), h_uint8(), NULL);
        default:
            return h_empty();
    }
}

// Define parser for Payload based on packet type
static HParser *mqtt_payload(uint8_t packet_type) {
    switch(packet_type) {
        case 1: // CONNECT
            return h_sequence(h_length_value(h_uint16(), h_bytes(0, h_uint16())), 
                              h_length_value(h_uint16(), h_bytes(0, h_uint16())), NULL);
        default:
            return h_empty();
    }
}

// Main MQTT packet parser
static HParser *mqtt_packet() {
    HParser *p_type = mqtt_control_packet_type();
    uint8_t packet_type = 0; // This needs dynamic handling based on actual packet type
    HParser *p_flags = mqtt_flags(packet_type);
    HParser *p_var_header = mqtt_variable_header(packet_type);
    HParser *p_payload = mqtt_payload(packet_type);

    return h_sequence(p_type, p_flags, p_var_header, p_payload, NULL);
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <mqtt_packet_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = (uint8_t *)malloc(length);
    if (data) {
        fread(data, 1, length, file);
    }
    fclose(file);

    HParser *mqtt_parser = mqtt_packet();
    HParseResult *result = h_parse(mqtt_parser, data, length);
    if (result) {
        printf("Parsed MQTT packet successfully.\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        printf("Failed to parse MQTT packet.\n");
    }

    free(data);
    h_parse_result_free(result);
    h_free_parser(mqtt_parser);

    return 0;
}