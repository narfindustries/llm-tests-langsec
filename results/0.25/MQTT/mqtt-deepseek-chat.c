#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define the MQTT Fixed Header
HParser *mqtt_fixed_header() {
    return h_sequence(
        h_bits(4, NULL), // MQTT Control Packet Type
        h_bits(4, NULL), // Flags
        h_length_value(h_uint8(), NULL), // Remaining Length
        NULL
    );
}

// Define the MQTT Variable Header
HParser *mqtt_variable_header() {
    return h_sequence(
        h_length_value(h_uint16(), NULL), // Protocol Name Length
        h_length_value(h_uint8(), NULL),  // Protocol Name
        h_uint8(),                        // Protocol Level
        h_uint8(),                        // Connect Flags
        h_uint16(),                       // Keep Alive
        NULL
    );
}

// Define the MQTT Payload
HParser *mqtt_payload() {
    return h_sequence(
        h_length_value(h_uint16(), NULL), // Client Identifier Length
        h_length_value(h_uint8(), NULL),  // Client Identifier
        NULL
    );
}

// Define the complete MQTT Packet
HParser *mqtt_packet() {
    return h_sequence(
        mqtt_fixed_header(),
        mqtt_variable_header(),
        mqtt_payload(),
        NULL
    );
}

int main(int argc, char **argv) {
    // Initialize the Hammer parser
    HParser *parser = mqtt_packet();

    // Read the input file
    FILE *file = fopen("input.mqtt", "rb");
    if (!file) {
        perror("Failed to open input file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    // Parse the input data
    HParseResult *result = h_parse(parser, data, file_size);
    if (!result) {
        fprintf(stderr, "Parsing failed\n");
        free(data);
        return 1;
    }

    // Print the parse result
    h_pprint(stdout, result->ast, 0, 0);
    printf("\n");

    // Clean up
    h_parse_result_free(result);
    free(data);

    return 0;
}