#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

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
} MQTTPacketType;

HParsedToken* parse_protocol_name(const HParseResult* p, void* user_data) {
    return (HParsedToken*)p->ast;
}

HParsedToken* parse_connect_flags(const HParseResult* p, void* user_data) {
    return (HParsedToken*)p->ast;
}

HParser* mqtt_variable_length_integer() {
    return h_many(
        h_sequence(
            h_bits(7, false),
            h_bits(1, false),
            NULL
        )
    );
}

HParser* mqtt_protocol_parser() {
    HParser *protocol_name = h_token("MQTT", 4);
    HParser *protocol_level = h_uint8();
    HParser *connect_flags = h_uint8();
    HParser *keep_alive = h_uint16();
    
    return h_sequence(
        protocol_name, 
        protocol_level, 
        connect_flags, 
        keep_alive, 
        NULL
    );
}

HParser* mqtt_packet_parser() {
    HParser *packet_type = h_bits(4, false);
    HParser *remaining_length = mqtt_variable_length_integer();
    
    HParser *connect_parser = h_action(mqtt_protocol_parser(), NULL, NULL);
    HParser *publish_parser = h_action(
        h_sequence(
            h_char_range('a', 'z'), 
            h_uint16(), 
            NULL
        ), 
        NULL, 
        NULL
    );
    
    return h_choice(
        h_sequence(
            packet_type, 
            remaining_length, 
            h_choice(
                connect_parser, 
                publish_parser, 
                NULL
            ), 
            NULL
        ),
        NULL
    );
}

int parse_mqtt_file(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Error opening file");
        return -1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return -1;
    }

    size_t read_size = fread(buffer, 1, file_size, file);
    fclose(file);

    if (read_size != file_size) {
        perror("File read error");
        free(buffer);
        return -1;
    }

    HParser *parser = mqtt_packet_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result && result->ast) {
        printf("MQTT packet parsed successfully\n");
        h_parse_result_free(result);
    } else {
        printf("MQTT packet parsing failed\n");
    }

    h_parser_destroy(parser);
    free(buffer);
    return 0;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <mqtt_file>\n", argv[0]);
        return 1;
    }

    return parse_mqtt_file(argv[1]);
}