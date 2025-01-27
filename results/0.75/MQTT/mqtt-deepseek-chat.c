#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define the MQTT fixed header structure
typedef struct {
    uint8_t type;
    uint8_t flags;
    uint32_t remaining_length;
} mqtt_fixed_header;

// Define the MQTT variable header structure
typedef struct {
    uint16_t packet_id;
} mqtt_variable_header;

// Define the MQTT payload structure
typedef struct {
    uint8_t *data;
    size_t length;
} mqtt_payload;

// Define the MQTT packet structure
typedef struct {
    mqtt_fixed_header fixed_header;
    mqtt_variable_header variable_header;
    mqtt_payload payload;
} mqtt_packet;

// Parser for MQTT fixed header
HParser *mqtt_fixed_header_parser() {
    return h_sequence(
        h_bits(4, &mqtt_fixed_header.type),  // MQTT packet type (4 bits)
        h_bits(4, &mqtt_fixed_header.flags), // MQTT flags (4 bits)
        h_uint32(&mqtt_fixed_header.remaining_length) // Remaining length (variable length)
    );
}

// Parser for MQTT variable header
HParser *mqtt_variable_header_parser() {
    return h_sequence(
        h_uint16(&mqtt_variable_header.packet_id) // Packet identifier (2 bytes)
    );
}

// Parser for MQTT payload
HParser *mqtt_payload_parser() {
    return h_many1(h_uint8(&mqtt_payload.data)); // Payload data (variable length)
}

// Parser for the complete MQTT packet
HParser *mqtt_packet_parser() {
    return h_sequence(
        mqtt_fixed_header_parser(),
        mqtt_variable_header_parser(),
        mqtt_payload_parser(),
        NULL
    );
}

// Main function to parse MQTT packets
int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(mqtt_packet_parser(), data, file_size);
    if (!result) {
        fprintf(stderr, "Failed to parse MQTT packet\n");
        free(data);
        return 1;
    }

    mqtt_packet *packet = (mqtt_packet *)result->ast;
    printf("Parsed MQTT packet:\n");
    printf("  Type: %d\n", packet->fixed_header.type);
    printf("  Flags: %d\n", packet->fixed_header.flags);
    printf("  Remaining Length: %d\n", packet->fixed_header.remaining_length);
    printf("  Packet ID: %d\n", packet->variable_header.packet_id);
    printf("  Payload Length: %zu\n", packet->payload.length);

    free(data);
    h_parse_result_free(result);

    return 0;
}