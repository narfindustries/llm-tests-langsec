#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define MQTT control packet types
#define CONNECT     1
#define CONNACK     2
#define PUBLISH     3
#define PUBACK      4
#define PUBREC      5
#define PUBREL      6
#define PUBCOMP     7
#define SUBSCRIBE   8
#define SUBACK      9
#define UNSUBSCRIBE 10
#define UNSUBACK    11
#define PINGREQ     12
#define PINGRESP    13
#define DISCONNECT  14
#define AUTH        15

// Parser for a single byte
static HParser *uint8;
// Parser for a two-byte integer
static HParser *uint16;
// Parser for a four-byte integer
static HParser *uint32;

// Parser for variable byte integer
static HParser *var_int() {
    return h_uint64();
}

// Parser for UTF-8 encoded strings
static HParser *utf8_string() {
    return h_length_value(uint16, h_bits(8 * h_length_value(uint16, h_any())));
}

// Parser for binary data
static HParser *binary_data() {
    return h_length_value(uint16, h_bits(8 * h_length_value(uint16, h_any())));
}

// Parser for MQTT properties
static HParser *mqtt_property() {
    return h_choice(
        h_sequence(h_int8(), uint8, NULL), // Payload Format Indicator
        h_sequence(h_int8(), uint32, NULL), // Message Expiry Interval
        h_sequence(h_int8(), utf8_string(), NULL), // Content Type
        h_sequence(h_int8(), utf8_string(), NULL), // Response Topic
        h_sequence(h_int8(), binary_data(), NULL), // Correlation Data
        h_sequence(h_int8(), var_int(), NULL), // Subscription Identifier
        h_sequence(h_int8(), uint32, NULL), // Session Expiry Interval
        h_sequence(h_int8(), utf8_string(), NULL), // Assigned Client Identifier
        h_sequence(h_int8(), uint16, NULL), // Server Keep Alive
        h_sequence(h_int8(), utf8_string(), NULL), // Authentication Method
        h_sequence(h_int8(), binary_data(), NULL), // Authentication Data
        h_sequence(h_int8(), uint8, NULL), // Request Problem Information
        h_sequence(h_int8(), uint32, NULL), // Will Delay Interval
        h_sequence(h_int8(), uint8, NULL), // Request Response Information
        h_sequence(h_int8(), utf8_string(), NULL), // Response Information
        h_sequence(h_int8(), utf8_string(), NULL), // Server Reference
        h_sequence(h_int8(), utf8_string(), NULL), // Reason String
        h_sequence(h_int8(), uint16, NULL), // Receive Maximum
        h_sequence(h_int8(), uint16, NULL), // Topic Alias Maximum
        h_sequence(h_int8(), uint16, NULL), // Topic Alias
        h_sequence(h_int8(), uint8, NULL), // Maximum QoS
        h_sequence(h_int8(), uint8, NULL), // Retain Available
        h_sequence(h_int8(), utf8_string(), utf8_string(), NULL), // User Property
        h_sequence(h_int8(), uint32, NULL), // Maximum Packet Size
        h_sequence(h_int8(), uint8, NULL), // Wildcard Subscription Available
        h_sequence(h_int8(), uint8, NULL), // Subscription Identifier Available
        h_sequence(h_int8(), uint8, NULL), // Shared Subscription Available
        NULL
    );
}

// Parser for MQTT packet
static HParser *mqtt_packet() {
    return h_sequence(
        uint8, // Packet type and flags
        var_int(), // Remaining length
        h_many(mqtt_property()), // Properties
        h_end_p(), // End of packet
        NULL
    );
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <MQTT binary file>\n", argv[0]);
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

    uint8_t *data = malloc(length);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, length, file);
    fclose(file);

    uint8 = h_uint8();
    uint16 = h_uint16();
    uint32 = h_uint32();

    HParseResult *result = h_parse(mqtt_packet(), data, length);
    if (result) {
        printf("MQTT packet parsed successfully.\n");
        h_pprint(stdout, result->ast, 0, 4);
    } else {
        printf("Failed to parse MQTT packet.\n");
    }

    h_parse_result_free(result);
    free(data);
    return 0;
}