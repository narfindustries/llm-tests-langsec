#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

typedef struct {
    uint8_t packet_type_flags;
    uint8_t remaining_length;
    // Additional fields depending on packet type
} mqtt_packet_t;

HParser *mqtt_fixed_header_parser() {
    HParser *packet_type_flags = h_bits(8, false);
    HParser *remaining_length = h_bits(8, false); // Simplified remaining length parsing

    return h_sequence(packet_type_flags, remaining_length, NULL);
}

HParser *mqtt_packet_parser() {
    return mqtt_fixed_header_parser();
}

void parse_mqtt_file(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Error opening file");
        exit(EXIT_FAILURE);
    }

    fseek(file, 0, SEEK_END);
    size_t filesize = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = (uint8_t *)malloc(filesize);
    if (!data) {
        perror("Error allocating memory");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    fread(data, 1, filesize, file);
    fclose(file);

    HParser *parser = mqtt_packet_parser();
    HParseResult *result = h_parse(parser, data, filesize);

    if (result->ast) {
        printf("MQTT packet parsed successfully.\n");
        // Further processing of the parsed packet
    } else {
        printf("Failed to parse MQTT packet.\n");
    }

    h_parse_result_free(result);
    free(data);
    h_parser_dec_ref(parser); // Corrected function to decrement parser reference count
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <mqtt-binary-file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    parse_mqtt_file(argv[1]);
    return EXIT_SUCCESS;
}