#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Forward declarations
HParser* init_mqtt_parser(void);
HParser* mqtt_fixed_header(void);
HParser* mqtt_variable_header(void);
HParser* mqtt_payload(void);
HParser* mqtt_properties(void);

// Property Parsers
HParser* property_single_byte(void) {
    return h_uint8();
}

HParser* property_two_byte(void) {
    return h_uint16();
}

HParser* property_four_byte(void) {
    return h_uint32();
}

HParser* property_var_int(void) {
    return h_many(h_uint8());
}

HParser* property_utf8_string(void) {
    return h_sequence(h_uint16(), h_many(h_uint8()), NULL);
}

HParser* property_binary_data(void) {
    return h_sequence(h_uint16(), h_many(h_uint8()), NULL);
}

HParser* mqtt_properties(void) {
    return h_sequence(
        h_uint8(),  // Property Length
        h_many(h_choice(
            h_sequence(h_ch(0x01), property_single_byte(), NULL),    // Payload Format Indicator
            h_sequence(h_ch(0x02), property_four_byte(), NULL),      // Message Expiry Interval
            h_sequence(h_ch(0x03), property_utf8_string(), NULL),    // Content Type
            h_sequence(h_ch(0x08), property_utf8_string(), NULL),    // Response Topic
            h_sequence(h_ch(0x09), property_binary_data(), NULL),    // Correlation Data
            h_sequence(h_ch(0x0B), property_var_int(), NULL),        // Subscription Identifier
            h_sequence(h_ch(0x11), property_four_byte(), NULL),      // Session Expiry Interval
            h_sequence(h_ch(0x12), property_utf8_string(), NULL),    // Assigned Client Identifier
            h_sequence(h_ch(0x13), property_two_byte(), NULL),       // Server Keep Alive
            h_sequence(h_ch(0x15), property_utf8_string(), NULL),    // Authentication Method
            h_sequence(h_ch(0x16), property_binary_data(), NULL),    // Authentication Data
            h_sequence(h_ch(0x17), property_single_byte(), NULL),    // Request Problem Information
            h_sequence(h_ch(0x19), property_single_byte(), NULL),    // Request Response Information
            h_sequence(h_ch(0x1A), property_utf8_string(), NULL),    // Response Information
            h_sequence(h_ch(0x1C), property_utf8_string(), NULL),    // Server Reference
            h_sequence(h_ch(0x1F), property_utf8_string(), NULL),    // Reason String
            h_sequence(h_ch(0x21), property_two_byte(), NULL),       // Receive Maximum
            h_sequence(h_ch(0x22), property_two_byte(), NULL),       // Topic Alias Maximum
            h_sequence(h_ch(0x23), property_two_byte(), NULL),       // Topic Alias
            h_sequence(h_ch(0x24), property_single_byte(), NULL),    // Maximum QoS
            h_sequence(h_ch(0x25), property_single_byte(), NULL),    // Retain Available
            h_sequence(h_ch(0x26), property_utf8_string(), NULL),    // User Property
            h_sequence(h_ch(0x27), property_four_byte(), NULL),      // Maximum Packet Size
            h_sequence(h_ch(0x28), property_single_byte(), NULL),    // Wildcard Subscription Available
            h_sequence(h_ch(0x29), property_single_byte(), NULL),    // Subscription Identifier Available
            h_sequence(h_ch(0x2A), property_single_byte(), NULL),    // Shared Subscription Available
            NULL)),
        NULL);
}

HParser* mqtt_fixed_header(void) {
    return h_sequence(
        h_bits(4, false),  // Packet Type
        h_bits(4, false),  // Flags
        h_many1(h_uint8()), // Remaining Length
        NULL);
}

HParser* mqtt_connect_variable_header(void) {
    return h_sequence(
        h_uint16(),        // Protocol Name Length
        h_many(h_uint8()), // Protocol Name
        h_uint8(),         // Protocol Version
        h_bits(8, false),  // Connect Flags
        h_uint16(),        // Keep Alive
        mqtt_properties(),
        NULL);
}

HParser* mqtt_connect_payload(void) {
    return h_sequence(
        h_uint16(),        // Client ID Length
        h_many(h_uint8()), // Client ID
        h_optional(h_sequence(
            mqtt_properties(),     // Will Properties
            h_uint16(),           // Will Topic Length
            h_many(h_uint8()),    // Will Topic
            h_uint16(),           // Will Payload Length
            h_many(h_uint8()),    // Will Payload
            NULL)),
        h_optional(h_sequence(
            h_uint16(),           // Username Length
            h_many(h_uint8()),    // Username
            NULL)),
        h_optional(h_sequence(
            h_uint16(),           // Password Length
            h_many(h_uint8()),    // Password
            NULL)),
        NULL);
}

HParser* mqtt_publish_variable_header(void) {
    return h_sequence(
        h_uint16(),        // Topic Name Length
        h_many(h_uint8()), // Topic Name
        h_optional(h_uint16()), // Packet Identifier (if QoS > 0)
        mqtt_properties(),
        NULL);
}

HParser* mqtt_publish_payload(void) {
    return h_many(h_uint8()); // Application Message
}

HParser* mqtt_subscribe_payload(void) {
    return h_many1(h_sequence(
        h_uint16(),        // Topic Filter Length
        h_many(h_uint8()), // Topic Filter
        h_uint8(),         // Subscription Options
        NULL));
}

HParser* init_mqtt_parser(void) {
    return h_sequence(
        mqtt_fixed_header(),
        h_choice(
            h_sequence(mqtt_connect_variable_header(), mqtt_connect_payload(), NULL),
            h_sequence(mqtt_publish_variable_header(), mqtt_publish_payload(), NULL),
            h_sequence(h_uint16(), mqtt_properties(), NULL), // For PUBACK, PUBREC, PUBREL, PUBCOMP
            h_sequence(h_uint16(), mqtt_properties(), mqtt_subscribe_payload(), NULL), // SUBSCRIBE
            h_sequence(h_uint16(), mqtt_properties(), NULL), // SUBACK
            h_sequence(h_uint16(), mqtt_properties(), h_many(h_uint8()), NULL), // UNSUBSCRIBE
            h_sequence(h_uint16(), mqtt_properties(), NULL), // UNSUBACK
            h_sequence(h_uint8(), mqtt_properties(), NULL), // DISCONNECT
            h_sequence(h_uint8(), mqtt_properties(), NULL), // AUTH
            NULL),
        NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
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

    uint8_t *input = malloc(file_size);
    if (!input) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    size_t bytes_read = fread(input, 1, file_size, file);
    fclose(file);

    if (bytes_read != file_size) {
        fprintf(stderr, "Failed to read entire file\n");
        free(input);
        return 1;
    }

    HParser *mqtt_parser = init_mqtt_parser();
    if (!mqtt_parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        free(input);
        return 1;
    }

    HParseResult *result = h_parse(mqtt_parser, input, file_size);
    if (!result) {
        fprintf(stderr, "Parse failed\n");
        free(input);
        return 1;
    }

    // Success
    fprintf(stdout, "Parse successful\n");
    
    h_parse_result_free(result);
    free(input);
    return 0;
}