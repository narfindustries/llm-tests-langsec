#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Forward declarations
static HParser* remaining_length_parser(void);
static HParser* connect_packet_parser(void);
static HParser* connack_packet_parser(void);
static HParser* publish_packet_parser(void);
static HParser* properties_parser(void);

// Utility parsers
static HParser* variable_byte_int(void) {
    return h_many(h_bits(8, false));
}

static HParser* utf8_string(void) {
    return h_sequence(h_uint16(), h_many(h_ch('\0')), NULL);
}

static HParser* binary_data(void) {
    return h_sequence(h_uint16(), h_many(h_bits(8, false)), NULL);
}

// Main MQTT parser
static HParser* mqtt_parser(void) {
    HParser* indirect_remaining = h_indirect();
    h_bind_indirect(indirect_remaining, remaining_length_parser());
    
    return h_sequence(
        h_bits(8, false),  // Packet type (4 bits) + Flags (4 bits)
        indirect_remaining,
        NULL
    );
}

// Remaining length parser
static HParser* remaining_length_parser(void) {
    return variable_byte_int();
}

// Properties parser
static HParser* properties_parser(void) {
    return h_sequence(
        variable_byte_int(),
        h_many(h_sequence(
            h_uint8(),
            h_choice(h_uint8(),
                    h_uint16(),
                    h_uint32(),
                    utf8_string(),
                    binary_data(),
                    variable_byte_int(),
                    NULL),
            NULL)),
        NULL
    );
}

// CONNECT packet parser
static HParser* connect_packet_parser(void) {
    return h_sequence(
        h_token((const uint8_t*)"MQTT", 4),
        h_uint8(),
        h_bits(8, false),
        h_uint16(),
        properties_parser(),
        utf8_string(),
        h_optional(utf8_string()),
        h_optional(utf8_string()),
        h_optional(binary_data()),
        h_optional(utf8_string()),
        h_optional(utf8_string()),
        NULL
    );
}

// CONNACK packet parser
static HParser* connack_packet_parser(void) {
    return h_sequence(
        h_bits(8, false),
        h_uint8(),
        properties_parser(),
        NULL
    );
}

// PUBLISH packet parser
static HParser* publish_packet_parser(void) {
    return h_sequence(
        utf8_string(),
        h_optional(h_uint16()),
        properties_parser(),
        h_many(h_bits(8, false)),
        NULL
    );
}

// PUBACK packet parser
static HParser* puback_packet_parser(void) {
    return h_sequence(
        h_uint16(),
        h_optional(h_uint8()),
        h_optional(properties_parser()),
        NULL
    );
}

// SUBSCRIBE packet parser
static HParser* subscribe_packet_parser(void) {
    return h_sequence(
        h_uint16(),
        properties_parser(),
        h_many1(h_sequence(
            utf8_string(),
            h_bits(8, false),
            NULL
        )),
        NULL
    );
}

// SUBACK packet parser
static HParser* suback_packet_parser(void) {
    return h_sequence(
        h_uint16(),
        properties_parser(),
        h_many1(h_uint8()),
        NULL
    );
}

// UNSUBSCRIBE packet parser
static HParser* unsubscribe_packet_parser(void) {
    return h_sequence(
        h_uint16(),
        properties_parser(),
        h_many1(utf8_string()),
        NULL
    );
}

// UNSUBACK packet parser
static HParser* unsuback_packet_parser(void) {
    return h_sequence(
        h_uint16(),
        properties_parser(),
        h_many1(h_uint8()),
        NULL
    );
}

// DISCONNECT packet parser
static HParser* disconnect_packet_parser(void) {
    return h_sequence(
        h_optional(h_uint8()),
        h_optional(properties_parser()),
        NULL
    );
}

// AUTH packet parser
static HParser* auth_packet_parser(void) {
    return h_sequence(
        h_optional(h_uint8()),
        h_optional(properties_parser()),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <mqtt_binary_file>\n", argv[0]);
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

    HParser *parser = mqtt_parser();
    HParseResult *result = h_parse(parser, input, size);

    if (result) {
        printf("Successfully parsed MQTT packet\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse MQTT packet\n");
    }

    free(input);
    fclose(f);
    return 0;
}