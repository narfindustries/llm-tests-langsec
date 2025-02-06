#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef struct {
    uint8_t type;
    uint8_t code;
    uint16_t checksum;
    union {
        struct {
            uint16_t identifier;
            uint16_t sequence;
        } echo;
        struct {
            uint32_t gateway;
        } redirect;
        struct {
            uint32_t unused;
            uint8_t data[64];
        } error;
    } payload;
} icmp_packet_t;

HParser *icmp_parser() {
    HParser *type = h_uint8();
    HParser *code = h_uint8();
    HParser *checksum = h_uint16();
    HParser *echo_id = h_uint16();
    HParser *echo_seq = h_uint16();
    HParser *redirect_gateway = h_uint32();
    HParser *error_unused = h_uint32();
    HParser *error_data = h_length_value(h_uint32(), h_uint8());

    HParser *echo_payload = h_sequence(echo_id, echo_seq, NULL);
    HParser *redirect_payload = h_sequence(redirect_gateway, NULL);
    HParser *error_payload = h_sequence(error_unused, error_data, NULL);

    HParser *payload = h_choice(
        h_sequence(h_uint8_eq(0), echo_payload, NULL),
        h_sequence(h_uint8_eq(8), echo_payload, NULL),
        h_sequence(h_uint8_eq(3), error_payload, NULL),
        h_sequence(h_uint8_eq(5), redirect_payload, NULL),
        h_sequence(h_uint8_eq(11), error_payload, NULL),
        NULL
    );

    return h_sequence(type, code, checksum, payload, NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
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

    uint8_t *buffer = (uint8_t *)malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = icmp_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        icmp_packet_t *packet = (icmp_packet_t *)result->ast;
        printf("Parsed ICMP packet:\n");
        printf("Type: %u\n", packet->type);
        printf("Code: %u\n", packet->code);
        printf("Checksum: %u\n", packet->checksum);
        if (packet->type == 0 || packet->type == 8) {
            printf("Identifier: %u\n", packet->payload.echo.identifier);
            printf("Sequence: %u\n", packet->payload.echo.sequence);
        } else if (packet->type == 5) {
            printf("Gateway: %u\n", packet->payload.redirect.gateway);
        } else if (packet->type == 3 || packet->type == 11) {
            printf("Unused: %u\n", packet->payload.error.unused);
            printf("Data: ");
            for (int i = 0; i < 64; i++) {
                printf("%02x ", packet->payload.error.data[i]);
            }
            printf("\n");
        }
        h_parse_result_free(result);
    } else {
        printf("Failed to parse ICMP packet.\n");
    }

    free(buffer);
    return 0;
}