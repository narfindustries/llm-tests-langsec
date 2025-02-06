#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Forward declarations
HParser* mqtt_parser(void);
HParser* fixed_header(void);
HParser* variable_header(void);
HParser* payload(void);
HParser* remaining_length(void);
HParser* properties(void);

// MQTT packet types
typedef enum {
    CONNECT = 1,
    CONNACK = 2,
    PUBLISH = 3,
    PUBACK = 4,
    PUBREC = 5,
    PUBREL = 6,
    PUBCOMP = 7,
    SUBSCRIBE = 8,
    SUBACK = 9,
    UNSUBSCRIBE = 10,
    UNSUBACK = 11,
    PINGREQ = 12,
    PINGRESP = 13,
    DISCONNECT = 14,
    AUTH = 15
} PacketType;

// Property identifiers
HParser* property_identifier(void) {
    return h_uint8();
}

// UTF-8 encoded string
HParser* utf8_string(void) {
    return h_sequence(h_uint16(), h_length_value(h_uint16(), h_uint8()), NULL);
}

// Variable byte integer
HParser* variable_byte_integer(void) {
    return h_many1(h_bits(7, false));
}

// Remaining length parser
HParser* remaining_length(void) {
    return variable_byte_integer();
}

// Properties parser
HParser* properties(void) {
    return h_sequence(
        variable_byte_integer(),  // Property Length
        h_many(h_sequence(
            property_identifier(),
            h_choice(h_uint8(), h_uint16(), h_uint32(), utf8_string(), NULL),
            NULL
        )),
        NULL
    );
}

// Fixed header parser
HParser* fixed_header(void) {
    return h_sequence(
        h_bits(4, false),  // Packet type
        h_bits(4, false),  // Flags
        remaining_length(),
        NULL
    );
}

// CONNECT packet parser
HParser* connect_packet(void) {
    return h_sequence(
        h_token((const uint8_t*)"MQTT", 4),  // Protocol name
        h_uint8(),                           // Protocol version
        h_bits(8, false),                    // Connect flags
        h_uint16(),                          // Keep alive
        properties(),                        // Properties
        utf8_string(),                       // Client ID
        h_optional(utf8_string()),           // Will Properties
        h_optional(utf8_string()),           // Will Topic
        h_optional(h_length_value(h_uint16(), h_uint8())), // Will Payload
        h_optional(utf8_string()),           // Username
        h_optional(utf8_string()),           // Password
        NULL
    );
}

// PUBLISH packet parser
HParser* publish_packet(void) {
    return h_sequence(
        utf8_string(),                       // Topic Name
        h_optional(h_uint16()),              // Packet Identifier
        properties(),                        // Properties
        h_many(h_uint8()),                   // Payload
        NULL
    );
}

// Main MQTT parser
HParser* mqtt_parser(void) {
    return h_sequence(
        fixed_header(),
        h_choice(
            connect_packet(),
            publish_packet(),
            // Add other packet types here
            NULL
        ),
        NULL
    );
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

    if (fread(input, 1, file_size, file) != (size_t)file_size) {
        perror("Failed to read file");
        free(input);
        fclose(file);
        return 1;
    }

    HParser *parser = mqtt_parser();
    HParseResult *result = h_parse(parser, input, file_size);

    if (result) {
        printf("Successfully parsed MQTT packet\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse MQTT packet\n");
    }

    free(input);
    fclose(file);
    return 0;
}