#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    uint8_t type;
    uint8_t code;
    uint16_t checksum;
    union {
        struct {
            uint16_t identifier;
            uint16_t sequence_number;
        } echo;
        struct {
            uint32_t gateway_internet_address;
        } redirect;
        struct {
            uint8_t pointer;
            uint8_t unused[3];
        } parameter_problem;
        struct {
            uint32_t originate_timestamp;
            uint32_t receive_timestamp;
            uint32_t transmit_timestamp;
        } timestamp;
    } data;
} ICMPMessage;

HParser *icmp_parser() {
    HParser *type = h_uint8();
    HParser *code = h_uint8();
    HParser *checksum = h_uint16();
    HParser *echo_id = h_uint16();
    HParser *echo_seq = h_uint16();
    HParser *gateway_addr = h_uint32();
    HParser *pointer = h_uint8();
    HParser *unused = h_repeat_n(h_uint8(), 3);
    HParser *originate_ts = h_uint32();
    HParser *receive_ts = h_uint32();
    HParser *transmit_ts = h_uint32();

    HParser *echo_data = h_sequence(echo_id, echo_seq, NULL);
    HParser *redirect_data = h_sequence(gateway_addr, NULL);
    HParser *parameter_problem_data = h_sequence(pointer, unused, NULL);
    HParser *timestamp_data = h_sequence(originate_ts, receive_ts, transmit_ts, NULL);

    HParser *icmp_message = h_sequence(
        type, code, checksum,
        h_choice(echo_data, redirect_data, parameter_problem_data, timestamp_data, NULL),
        NULL
    );

    return icmp_message;
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

    uint8_t *buffer = malloc(file_size);
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
        ICMPMessage *msg = (ICMPMessage *)result->ast;
        printf("Type: %d\n", msg->type);
        printf("Code: %d\n", msg->code);
        printf("Checksum: %d\n", msg->checksum);

        switch (msg->type) {
            case 0: case 8:
                printf("Identifier: %d\n", msg->data.echo.identifier);
                printf("Sequence Number: %d\n", msg->data.echo.sequence_number);
                break;
            case 5:
                printf("Gateway Internet Address: %u\n", msg->data.redirect.gateway_internet_address);
                break;
            case 12:
                printf("Pointer: %d\n", msg->data.parameter_problem.pointer);
                break;
            case 13: case 14:
                printf("Originate Timestamp: %u\n", msg->data.timestamp.originate_timestamp);
                printf("Receive Timestamp: %u\n", msg->data.timestamp.receive_timestamp);
                printf("Transmit Timestamp: %u\n", msg->data.timestamp.transmit_timestamp);
                break;
        }

        h_parse_result_free(result);
    } else {
        printf("Failed to parse ICMP message\n");
    }

    free(buffer);
    return 0;
}