#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Declare MQTT fields
HParser *utf8_string = h_sequence(h_uint16(), h_data(h_last_uint()), NULL);
HParser *binary_data = h_sequence(h_uint16(), h_data(h_last_uint()), NULL);

// Substitute variable_int with appropriate sequence
HParser *variable_int = h_many1(h_uint8_t(bit(n:7) & ((n & 0x80) ? true : false)));

HParser *packet_id = h_uint16();
HParser *reason_code = h_uint8();

// Protocol Fields
HParser *protocol_name = h_sequence(
  h_uint16(),
  h_token((const uint8_t*)"MQTT", 4),
  NULL);

HParser *protocol_level = h_uint8();

// Connect Flags
HParser *connect_flags = h_uint8();

// Properties and optional fields
HParser *property = h_choice(
  h_sequence(h_uint8(), h_optional(binary_data), NULL),
  NULL);

HParser *properties = h_many(property);

// CONNECT Packet
HParser *connect_packet = h_sequence(
  protocol_name,
  protocol_level,
  connect_flags,
  h_uint16(), // Keep Alive
  properties,
  utf8_string, // Client Identifier
  h_optional(properties),
  h_optional(utf8_string), // Will Topic
  h_optional(binary_data), // Will Payload
  h_optional(utf8_string), // User Name
  h_optional(binary_data), // Password
  NULL);

// PUBLISH Packet
HParser *publish_packet = h_sequence(
  h_uint8(), // DUP, QoS, RETAIN
  utf8_string, // Topic Name
  h_optional(packet_id), // Packet Identifier if QoS > 0
  properties,
  binary_data, // Payload
  NULL);

// PUBACK Packet
HParser *puback_packet = h_sequence(
  packet_id,
  reason_code,
  properties,
  NULL);

// SUBSCRIBE Packet
HParser *subscribe_packet = h_sequence(
  packet_id,
  properties,
  h_many(
    h_sequence(
      utf8_string,
      h_uint8(), // Requested QoS
      NULL)),
  NULL);

// SUBACK Packet
HParser *suback_packet = h_sequence(
  packet_id,
  properties,
  h_many(reason_code),
  NULL);

// UNSUBSCRIBE Packet
HParser *unsubscribe_packet = h_sequence(
  packet_id,
  properties,
  h_many(utf8_string),
  NULL);

// UNSUBACK Packet
HParser *unsuback_packet = h_sequence(
  packet_id,
  properties,
  h_many(reason_code),
  NULL);

// DISCONNECT Packet
HParser *disconnect_packet = h_sequence(
  reason_code,
  properties,
  NULL);

// AUTH Packet
HParser *auth_packet = h_sequence(
  reason_code,
  properties,
  NULL);

// Main MQTT Parser
HParser *mqtt_parser = h_choice(
  connect_packet,
  publish_packet,
  puback_packet,
  subscribe_packet,
  suback_packet,
  unsubscribe_packet,
  unsuback_packet,
  disconnect_packet,
  auth_packet,
  NULL);

void parse_file(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        fprintf(stderr, "Error opening file: %s\n", filename);
        exit(EXIT_FAILURE);
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *buffer = malloc(file_size);
    if (!buffer) {
        fprintf(stderr, "Memory allocation failed\n");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        fprintf(stderr, "Error reading file\n");
        free(buffer);
        fclose(file);
        exit(EXIT_FAILURE);
    }

    fclose(file);

    HParseResult *result = h_parse(mqtt_parser, buffer, file_size);
    if (result) {
        printf("Parsed successfully!\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse the MQTT message\n");
    }

    free(buffer);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    parse_file(argv[1]);

    return EXIT_SUCCESS;
}