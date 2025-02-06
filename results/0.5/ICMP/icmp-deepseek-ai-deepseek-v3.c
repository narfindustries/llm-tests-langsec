#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    uint8_t type;
    uint8_t code;
    uint16_t checksum;
    union {
        struct {
            uint16_t id;
            uint16_t seq;
        } echo;
        struct {
            uint32_t gateway;
        } redirect;
        struct {
            uint8_t pointer;
            uint8_t unused[3];
        } param_problem;
        struct {
            uint8_t unused[4];
        } other;
    } data;
    uint8_t original_data[8];
} ICMPPacket;

HParser *icmp_parser() {
    return h_sequence(
        h_uint8(),  // type
        h_uint8(),  // code
        h_uint16(), // checksum
        h_choice(
            h_sequence(h_uint16(), h_uint16()), // echo
            h_sequence(h_uint32()),           // redirect
            h_sequence(h_uint8(), h_bits(24, false)), // param_problem
            h_bits(32, false),                       // other
            NULL
        ),
        h_bits(64, false), // original_data
        NULL
    );
}

void parse_icmp(const uint8_t *data, size_t size) {
    HParser *parser = icmp_parser();
    HParseResult *result = h_parse(parser, data, size);
    if (result) {
        ICMPPacket *packet = (ICMPPacket *)result->ast;
        printf("Type: %d\n", packet->type);
        printf("Code: %d\n", packet->code);
        printf("Checksum: %d\n", packet->checksum);
        if (packet->type == 8 || packet->type == 0) {
            printf("ID: %d\n", packet->data.echo.id);
            printf("Sequence: %d\n", packet->data.echo.seq);
        } else if (packet->type == 5) {
            printf("Gateway: %d\n", packet->data.redirect.gateway);
        } else if (packet->type == 12) {
            printf("Pointer: %d\n", packet->data.param_problem.pointer);
        }
        printf("Original Data: ");
        for (int i = 0; i < 8; i++) {
            printf("%02x ", packet->original_data[i]);
        }
        printf("\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse ICMP packet.\n");
    }
    h_arena_free(NULL, parser);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s <binary file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = (uint8_t *)malloc(size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, size, file);
    fclose(file);

    parse_icmp(data, size);
    free(data);

    return 0;
}