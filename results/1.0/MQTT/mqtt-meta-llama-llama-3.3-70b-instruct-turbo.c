#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// MQTT protocol specification
// https://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html

// Header
typedef struct {
    uint8_t header;
    uint8_t remaining_length;
} mqtt_header_t;

// Connect packet
typedef struct {
    uint16_t protocol_name_length;
    char protocol_name[4];
    uint8_t protocol_version;
    uint8_t connect_flags;
    uint8_t keep_alive;
} mqtt_connect_t;

// Connect ack packet
typedef struct {
    uint8_t session_present;
    uint8_t connect_return_code;
} mqtt_connect_ack_t;

// Publish packet
typedef struct {
    uint16_t topic_name_length;
    char topic_name[256];
    uint16_t packet_id;
    char payload[1024];
} mqtt_publish_t;

// Puback packet
typedef struct {
    uint16_t packet_id;
} mqtt_puback_t;

// Message structure
typedef struct {
    mqtt_header_t header;
    uint8_t packet_type;
    union {
        mqtt_connect_t connect;
        mqtt_connect_ack_t connect_ack;
        mqtt_publish_t publish;
        mqtt_puback_t puback;
    } packet;
} mqtt_message_t;

// Parser function
int parse_mqtt_message(uint8_t* buffer, size_t length, mqtt_message_t* message) {
    // Parse header
    message->header.header = buffer[0];
    message->header.remaining_length = buffer[1];

    // Determine packet type
    message->packet_type = (message->header.header >> 4) & 0x0F;

    // Parse packet based on type
    switch (message->packet_type) {
        case 0x01: // Connect
            // Parse connect packet
            message->packet.connect.protocol_name_length = (uint16_t)((buffer[2] << 8) | buffer[3]);
            memcpy(message->packet.connect.protocol_name, &buffer[4], 4);
            message->packet.connect.protocol_version = buffer[8];
            message->packet.connect.connect_flags = buffer[9];
            message->packet.connect.keep_alive = (uint8_t)((buffer[10] << 8) | buffer[11]);
            break;
        case 0x02: // Connect ack
            // Parse connect ack packet
            message->packet.connect_ack.session_present = buffer[2];
            message->packet.connect_ack.connect_return_code = buffer[3];
            break;
        case 0x03: // Publish
            // Parse publish packet
            message->packet.publish.topic_name_length = (uint16_t)((buffer[2] << 8) | buffer[3]);
            memcpy(message->packet.publish.topic_name, &buffer[4], 256);
            message->packet.publish.packet_id = (uint16_t)((buffer[260] << 8) | buffer[261]);
            memcpy(message->packet.publish.payload, &buffer[262], 1024);
            break;
        case 0x04: // Puback
            // Parse puback packet
            message->packet.puback.packet_id = (uint16_t)((buffer[2] << 8) | buffer[3]);
            break;
        default:
            // Unknown packet type
            return -1;
    }

    return 0;
}

int main() {
    uint8_t buffer[1024];
    mqtt_message_t message;

    // Generate sample MQTT message
    buffer[0] = 0x10; // Header
    buffer[1] = 0x0F; // Remaining length
    buffer[2] = 0x00; // Protocol name length MSB
    buffer[3] = 0x04; // Protocol name length LSB
    memcpy(&buffer[4], "MQTT", 4); // Protocol name
    buffer[8] = 0x04; // Protocol version
    buffer[9] = 0x02; // Connect flags
    buffer[10] = 0x00; // Keep alive MSB
    buffer[11] = 0x0A; // Keep alive LSB

    // Parse MQTT message
    int result = parse_mqtt_message(buffer, 12, &message);
    if (result != 0) {
        printf("Error parsing MQTT message\n");
        return 1;
    }

    // Print parsed message
    printf("Header: 0x%02X\n", message.header.header);
    printf("Remaining length: 0x%02X\n", message.header.remaining_length);
    printf("Packet type: 0x%02X\n", message.packet_type);

    switch (message.packet_type) {
        case 0x01: // Connect
            printf("Protocol name: %s\n", message.packet.connect.protocol_name);
            printf("Protocol version: 0x%02X\n", message.packet.connect.protocol_version);
            printf("Connect flags: 0x%02X\n", message.packet.connect.connect_flags);
            printf("Keep alive: 0x%02X\n", message.packet.connect.keep_alive);
            break;
        case 0x02: // Connect ack
            printf("Session present: 0x%02X\n", message.packet.connect_ack.session_present);
            printf("Connect return code: 0x%02X\n", message.packet.connect_ack.connect_return_code);
            break;
        case 0x03: // Publish
            printf("Topic name: %s\n", message.packet.publish.topic_name);
            printf("Packet ID: 0x%04X\n", message.packet.publish.packet_id);
            printf("Payload: %s\n", message.packet.publish.payload);
            break;
        case 0x04: // Puback
            printf("Packet ID: 0x%04X\n", message.packet.puback.packet_id);
            break;
        default:
            printf("Unknown packet type\n");
            break;
    }

    return 0;
}