#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

void init_parsers(HParser **byte_parser, HParser **uint16_parser, HParser **uint32_parser, HParser **string_parser, HParser **binary_data_parser, HParser **property_parser, HParser **mqtt_packet_parser) {
    *byte_parser = h_uint8();
    *uint16_parser = h_uint16();
    *uint32_parser = h_uint32();
    *string_parser = h_length_value(h_uint16(), h_sequence(h_uint8(), NULL));
    *binary_data_parser = h_length_value(h_uint16(), h_sequence(h_uint8(), NULL));

    *property_parser = h_choice(
        h_sequence(h_uint8(), h_uint8(), NULL), // Payload Format Indicator
        h_sequence(h_uint8(), *uint32_parser, NULL), // Message Expiry Interval
        h_sequence(h_uint8(), *string_parser, NULL), // Content Type
        h_sequence(h_uint8(), *string_parser, NULL), // Response Topic
        h_sequence(h_uint8(), *binary_data_parser, NULL), // Correlation Data
        h_sequence(h_uint8(), h_length_value(h_uint8(), h_sequence(h_uint8(), NULL)), NULL), // Subscription Identifier
        h_sequence(h_uint8(), *uint32_parser, NULL), // Session Expiry Interval
        h_sequence(h_uint8(), *string_parser, NULL), // Assigned Client Identifier
        h_sequence(h_uint8(), *uint16_parser, NULL), // Server Keep Alive
        h_sequence(h_uint8(), *string_parser, NULL), // Authentication Method
        h_sequence(h_uint8(), *binary_data_parser, NULL), // Authentication Data
        h_sequence(h_uint8(), *byte_parser, NULL), // Request Problem Information
        h_sequence(h_uint8(), *uint32_parser, NULL), // Will Delay Interval
        h_sequence(h_uint8(), *byte_parser, NULL), // Request Response Information
        h_sequence(h_uint8(), *string_parser, NULL), // Response Information
        h_sequence(h_uint8(), *string_parser, NULL), // Server Reference
        h_sequence(h_uint8(), *string_parser, NULL), // Reason String
        h_sequence(h_uint8(), *uint16_parser, NULL), // Receive Maximum
        h_sequence(h_uint8(), *uint16_parser, NULL), // Topic Alias Maximum
        h_sequence(h_uint8(), *uint16_parser, NULL), // Topic Alias
        h_sequence(h_uint8(), *byte_parser, NULL), // Maximum QoS
        h_sequence(h_uint8(), *byte_parser, NULL), // Retain Available
        h_sequence(h_uint8(), h_sequence(*string_parser, *string_parser, NULL), NULL), // User Property
        h_sequence(h_uint8(), *uint32_parser, NULL), // Maximum Packet Size
        h_sequence(h_uint8(), *byte_parser, NULL), // Wildcard Subscription Available
        h_sequence(h_uint8(), *byte_parser, NULL), // Subscription Identifier Available
        h_sequence(h_uint8(), *byte_parser, NULL), // Shared Subscription Available
        NULL
    );

    *mqtt_packet_parser = h_choice(
        h_sequence(h_uint8(), h_uint8(), *string_parser, *uint16_parser, *byte_parser, *uint16_parser, *property_parser, *string_parser, *string_parser, *binary_data_parser, *string_parser, *binary_data_parser, NULL), // CONNECT
        h_sequence(h_uint8(), *byte_parser, *property_parser, NULL), // CONNACK
        h_sequence(h_uint8(), *byte_parser, *string_parser, *uint16_parser, *property_parser, *binary_data_parser, NULL), // PUBLISH
        h_sequence(h_uint8(), *uint16_parser, *byte_parser, *property_parser, NULL), // PUBACK
        h_sequence(h_uint8(), *uint16_parser, *byte_parser, *property_parser, NULL), // PUBREC
        h_sequence(h_uint8(), *uint16_parser, *byte_parser, *property_parser, NULL), // PUBREL
        h_sequence(h_uint8(), *uint16_parser, *byte_parser, *property_parser, NULL), // PUBCOMP
        h_sequence(h_uint8(), *uint16_parser, *property_parser, h_sequence(*string_parser, *byte_parser, NULL), NULL), // SUBSCRIBE
        h_sequence(h_uint8(), *uint16_parser, *property_parser, h_sequence(*byte_parser, NULL), NULL), // SUBACK
        h_sequence(h_uint8(), *uint16_parser, *property_parser, h_sequence(*string_parser, NULL), NULL), // UNSUBSCRIBE
        h_sequence(h_uint8(), *uint16_parser, *property_parser, h_sequence(*byte_parser, NULL), NULL), // UNSUBACK
        h_sequence(h_uint8(), NULL), // PINGREQ
        h_sequence(h_uint8(), NULL), // PINGRESP
        h_sequence(h_uint8(), *byte_parser, *property_parser, NULL), // DISCONNECT
        h_sequence(h_uint8(), *byte_parser, *property_parser, NULL), // AUTH
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
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *byte_parser, *uint16_parser, *uint32_parser, *string_parser, *binary_data_parser, *property_parser, *mqtt_packet_parser;
    init_parsers(&byte_parser, &uint16_parser, &uint32_parser, &string_parser, &binary_data_parser, &property_parser, &mqtt_packet_parser);

    HParseResult *result = h_parse(mqtt_packet_parser, buffer, file_size);
    if (result) {
        printf("Parsing successful!\n");
    } else {
        printf("Parsing failed!\n");
    }

    free(buffer);
    return 0;
}