#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Parser for a single byte
HParser* uint8() {
    return h_uint8();
}

// Parser for two bytes interpreted as uint16
HParser* uint16() {
    return h_bits(16, false);
}

// Parser for four bytes interpreted as uint32
HParser* uint32() {
    return h_bits(32, false);
}

// Parser for UTF-8 encoded strings prefixed with length
HParser* mqtt_string() {
    return h_length_value(uint16(), h_uint8());
}

// Parser for Variable Byte Integer (used in Remaining Length field)
HParser* var_byte_int() {
    return h_many1(h_uint8());
}

// Parser for MQTT Properties
HParser* mqtt_properties() {
    return h_length_value(var_byte_int(), h_uint8()); // Simplified for example
}

// MQTT Control Packet Types
HParser* connect_packet() {
    HParser* protocol_name = mqtt_string();
    HParser* protocol_level = uint8();
    HParser* connect_flags = uint8();
    HParser* keep_alive = uint16();
    HParser* properties = mqtt_properties();
    HParser* client_id = mqtt_string();
    HParser* will_properties = mqtt_properties();
    HParser* will_topic = mqtt_string();
    HParser* will_payload = mqtt_string();
    HParser* username = mqtt_string();
    HParser* password = mqtt_string();

    return h_sequence(protocol_name, protocol_level, connect_flags, keep_alive, properties, client_id, will_properties, will_topic, will_payload, username, password, NULL);
}

HParser* connack_packet() {
    HParser* ack_flags = uint8();
    HParser* reason_code = uint8();
    HParser* properties = mqtt_properties();

    return h_sequence(ack_flags, reason_code, properties, NULL);
}

HParser* publish_packet() {
    HParser* topic_name = mqtt_string();
    HParser* packet_id = uint16();
    HParser* properties = mqtt_properties();
    HParser* payload = h_many(h_uint8());

    return h_sequence(topic_name, packet_id, properties, payload, NULL);
}

// Main function to parse input file based on MQTT specification
int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <mqtt_binary_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    // Seek to the end of the file to determine the size
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    // Read the entire file into memory
    unsigned char *data = malloc(file_size);
    if (fread(data, 1, file_size, file) != (size_t)file_size) {
        fprintf(stderr, "Failed to read the file\n");
        fclose(file);
        free(data);
        return 1;
    }
    fclose(file);

    // Choose the correct parser based on your application (assuming CONNECT for demonstration)
    HParser* mqtt_parser = connect_packet();

    HParseResult *result = h_parse(mqtt_parser, data, file_size);
    if (result) {
        printf("Parse successful!\n");
        // Here you would ideally pretty-print or process the parse result
    } else {
        printf("Parse failed!\n");
    }

    // Cleanup
    h_parse_result_free(result);
    free(data);
    h_free_parser(mqtt_parser);

    return 0;
}