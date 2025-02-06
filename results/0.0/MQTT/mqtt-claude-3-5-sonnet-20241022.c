#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Forward declarations
HParser* mqtt_parser(void);
HParser* fixed_header(void);
HParser* variable_header(void);
HParser* payload(void);
HParser* properties(void);

// Helper parsers
HParser* variable_byte_int(void) {
    return h_many(h_bits(7, false));
}

HParser* utf8_string(void) {
    return h_sequence(h_uint16(), h_length_value(h_uint16(), h_uint8()));
}

HParser* binary_data(void) {
    return h_sequence(h_uint16(), h_length_value(h_uint16(), h_uint8()));
}

// Property parsers
HParser* property(void) {
    return h_choice(
        h_sequence(h_ch(0x01), h_uint8()),     // Payload Format Indicator
        h_sequence(h_ch(0x02), h_uint32()),    // Message Expiry Interval
        h_sequence(h_ch(0x03), utf8_string()), // Content Type
        h_sequence(h_ch(0x08), utf8_string()), // Response Topic
        h_sequence(h_ch(0x09), binary_data()), // Correlation Data
        h_sequence(h_ch(0x0B), variable_byte_int()), // Subscription Identifier
        h_sequence(h_ch(0x11), h_uint32()),    // Session Expiry Interval
        h_sequence(h_ch(0x15), utf8_string()), // Authentication Method
        h_sequence(h_ch(0x16), binary_data()), // Authentication Data
        h_sequence(h_ch(0x17), h_uint8()),     // Request Problem Information
        h_sequence(h_ch(0x19), h_uint8()),     // Request Response Information
        h_sequence(h_ch(0x21), h_uint16()),    // Receive Maximum
        h_sequence(h_ch(0x22), h_uint16()),    // Topic Alias Maximum
        h_sequence(h_ch(0x23), h_uint16()),    // Topic Alias
        h_sequence(h_ch(0x26), h_repeat_n(utf8_string(), 2)), // User Properties
        h_sequence(h_ch(0x27), h_uint32())     // Maximum Packet Size
    );
}

HParser* properties(void) {
    return h_sequence(
        variable_byte_int(),
        h_many(property())
    );
}

// Fixed Header parser
HParser* fixed_header(void) {
    return h_sequence(
        h_bits(4, false),  // Packet Type
        h_bits(4, false),  // Flags
        variable_byte_int() // Remaining Length
    );
}

// CONNECT packet specific parsers
HParser* connect_flags(void) {
    return h_bits(8, false);
}

HParser* connect_variable_header(void) {
    return h_sequence(
        utf8_string(),     // Protocol Name
        h_uint8(),         // Protocol Version
        connect_flags(),   // Connect Flags
        h_uint16(),        // Keep Alive
        properties()       // Properties
    );
}

HParser* connect_payload(void) {
    return h_sequence(
        utf8_string(),     // Client Identifier
        h_optional(h_sequence(
            properties(),  // Will Properties
            utf8_string(), // Will Topic
            binary_data()  // Will Payload
        )),
        h_optional(utf8_string()), // Username
        h_optional(utf8_string())  // Password
    );
}

// PUBLISH packet specific parsers
HParser* publish_variable_header(void) {
    return h_sequence(
        utf8_string(),     // Topic Name
        h_optional(h_uint16()), // Packet Identifier (if QoS > 0)
        properties()       // Properties
    );
}

// Main MQTT parser
HParser* mqtt_parser(void) {
    return h_sequence(
        fixed_header(),
        h_choice(
            h_sequence(connect_variable_header(), connect_payload()),
            publish_variable_header(),
            properties()
        ),
        h_optional(h_many(h_uint8())) // Optional payload
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t *input = malloc(size);
    if (!input) {
        perror("Failed to allocate memory");
        fclose(f);
        return 1;
    }

    if (fread(input, 1, size, f) != size) {
        perror("Failed to read file");
        free(input);
        fclose(f);
        return 1;
    }
    fclose(f);

    HParser *parser = mqtt_parser();
    HParseResult *result = h_parse(parser, input, size);

    if (result) {
        printf("Successfully parsed MQTT packet\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse MQTT packet\n");
    }

    free(input);
    return 0;
}