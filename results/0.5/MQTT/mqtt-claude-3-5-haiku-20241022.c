#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef enum {
    CONNECT = 1,
    CONNACK,
    PUBLISH,
    PUBACK,
    PUBREC,
    PUBREL,
    PUBCOMP,
    SUBSCRIBE,
    SUBACK,
    UNSUBSCRIBE,
    UNSUBACK,
    PINGREQ,
    PINGRESP,
    DISCONNECT,
    AUTH
} MQTTPacketType;

HParser* mqtt_variable_length_integer() {
    return h_choice(
        h_uint8(),
        h_sequence(
            h_uint8(),
            h_uint8(),
            NULL
        ),
        h_sequence(
            h_uint8(),
            h_uint8(),
            h_uint8(),
            NULL
        ),
        h_sequence(
            h_uint8(),
            h_uint8(),
            h_uint8(),
            h_uint8(),
            NULL
        ),
        NULL
    );
}

HParser* mqtt_fixed_header() {
    return h_sequence(
        h_bits(4, false),
        h_bits(4, false),
        mqtt_variable_length_integer(),
        NULL
    );
}

HParser* mqtt_connect_packet() {
    return h_sequence(
        h_token("MQTT", 4),
        h_uint8(),
        h_bits(8, false),
        h_uint16(),
        h_length_value(h_uint8(), h_many(h_ch('a'))),
        NULL
    );
}

HParser* mqtt_parser() {
    return h_choice(
        mqtt_connect_packet(),
        NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <mqtt_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("File read error");
        free(buffer);
        fclose(file);
        return 1;
    }
    fclose(file);

    HParser* parser = mqtt_parser();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result && result->ast) {
        printf("MQTT packet parsed successfully\n");
    } else {
        printf("MQTT packet parsing failed\n");
    }

    h_parse_result_free(result);
    h_parser_free(parser);
    free(buffer);

    return 0;
}