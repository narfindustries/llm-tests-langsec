#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Forward declarations
HParser* mqtt_parser(void);
HParser* fixed_header(void);
HParser* variable_header(void);
HParser* properties(void);
HParser* payload(void);

// Helper parsers
HParser* variable_byte_int(void) {
    return h_sequence(h_bits(8, false), NULL);
}

HParser* utf8_string(void) {
    return h_sequence(h_uint16(), h_many1(h_ch_range(0x00, 0xFF)), NULL);
}

HParser* binary_data(void) {
    return h_sequence(h_uint16(), h_many1(h_ch_range(0x00, 0xFF)), NULL);
}

// Fixed header parser
HParser* fixed_header(void) {
    HParser* packet_type = h_bits(4, false);
    HParser* flags = h_bits(4, false);
    HParser* remaining_length = variable_byte_int();
    
    return h_sequence(packet_type, flags, remaining_length, NULL);
}

// Property parser
HParser* property(void) {
    HParser* property_identifier = h_uint8();
    HParser* property_value = h_choice(
        h_uint8(),
        h_uint16(),
        h_uint32(),
        utf8_string(),
        binary_data(),
        variable_byte_int(),
        NULL
    );
    
    return h_sequence(property_identifier, property_value, NULL);
}

// Properties section parser
HParser* properties(void) {
    HParser* property_length = variable_byte_int();
    HParser* property_list = h_many(property());
    
    return h_sequence(property_length, property_list, NULL);
}

// CONNECT packet specific parsers
HParser* connect_flags(void) {
    return h_bits(8, false);
}

HParser* connect_variable_header(void) {
    HParser* protocol_name = utf8_string();
    HParser* protocol_version = h_uint8();
    HParser* connect_flags_parser = connect_flags();
    HParser* keep_alive = h_uint16();
    
    return h_sequence(protocol_name, protocol_version, 
                     connect_flags_parser, keep_alive, 
                     properties(), NULL);
}

// PUBLISH packet specific parsers
HParser* publish_variable_header(void) {
    HParser* topic_name = utf8_string();
    HParser* packet_identifier = h_optional(h_uint16());
    
    return h_sequence(topic_name, packet_identifier, properties(), NULL);
}

// Variable header parser
HParser* variable_header(void) {
    return h_choice(
        connect_variable_header(),
        publish_variable_header(),
        NULL
    );
}

// Payload parser
HParser* payload(void) {
    return h_many(h_uint8());
}

// Main MQTT packet parser
HParser* mqtt_parser(void) {
    return h_sequence(fixed_header(), variable_header(), payload(), NULL);
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* input = malloc(size);
    if (!input) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if (fread(input, 1, size, file) != size) {
        perror("Failed to read file");
        free(input);
        fclose(file);
        return 1;
    }

    fclose(file);

    HParser* mqtt = mqtt_parser();
    HParseResult* result = h_parse(mqtt, input, size);

    if (result) {
        printf("Successfully parsed MQTT packet\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse MQTT packet\n");
    }

    free(input);
    return 0;
}