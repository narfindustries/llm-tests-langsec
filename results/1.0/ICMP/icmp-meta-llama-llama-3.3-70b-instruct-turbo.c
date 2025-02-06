#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define ICMP_TYPE_ECHO_REPLY 0
#define ICMP_TYPE_REDIRECT 5
#define ICMP_TYPE_ECHO_REQUEST 8
#define ICMP_TYPE_TIME_EXCEEDED 11
#define ICMP_TYPE_PARAMETER_PROBLEM 12
#define ICMP_TYPE_TIMESTAMP_REQUEST 13
#define ICMP_TYPE_TIMESTAMP_REPLY 14

#define ICMP_CODE_REDIRECT_NETWORK 0
#define ICMP_CODE_REDIRECT_HOST 1
#define ICMP_CODE_REDIRECT_TYPE_OF_SERVICE_AND_NETWORK 2
#define ICMP_CODE_REDIRECT_TYPE_OF_SERVICE_AND_HOST 3
#define ICMP_CODE_TIME_EXCEEDED_TTL 0
#define ICMP_CODE_TIME_EXCEEDED_FRAG 1
#define ICMP_CODE_PARAM_PROBLEM_POINTER 0
#define ICMP_CODE_PARAM_PROBLEM_MISSING 1

typedef struct {
    uint8_t* data;
    size_t size;
} input_t;

typedef struct {
    uint8_t type;
    uint8_t code;
    uint16_t checksum;
    union {
        struct {
            uint16_t identifier;
            uint16_t sequence_number;
        } echo;
        uint32_t gateway_address;
        uint32_t unused;
        struct {
            uint16_t identifier;
            uint16_t sequence_number;
            uint32_t timestamp;
        } timestamp;
    } data;
    uint8_t* rest;
} icmp_packet_t;

int icmp_parse(input_t* input, void* user, icmp_packet_t** packet) {
    uint8_t type, code;
    uint16_t checksum;

    if (input->size < 8) {
        return 0;
    }

    type = input->data[0];
    code = input->data[1];
    checksum = (input->data[2] << 8) | input->data[3];

    if (input->size < 8) {
        return 0;
    }

    icmp_packet_t* pkt = malloc(sizeof(icmp_packet_t));

    pkt->type = type;
    pkt->code = code;
    pkt->checksum = checksum;

    uint8_t* pos = input->data + 4;

    switch (type) {
        case ICMP_TYPE_ECHO_REQUEST:
        case ICMP_TYPE_ECHO_REPLY:
            if (input->size < 8) {
                free(pkt);
                return 0;
            }

            pkt->data.echo.identifier = (pos[0] << 8) | pos[1];
            pkt->data.echo.sequence_number = (pos[2] << 8) | pos[3];

            pkt->rest = pos + 4;
            break;

        case ICMP_TYPE_REDIRECT:
            if (input->size < 8) {
                free(pkt);
                return 0;
            }

            pkt->data.gateway_address = (pos[0] << 24) | (pos[1] << 16) | (pos[2] << 8) | pos[3];

            pkt->rest = pos + 4;
            break;

        case ICMP_TYPE_TIME_EXCEEDED:
        case ICMP_TYPE_PARAMETER_PROBLEM:
            if (input->size < 8) {
                free(pkt);
                return 0;
            }

            pkt->data.unused = (pos[0] << 24) | (pos[1] << 16) | (pos[2] << 8) | pos[3];

            pkt->rest = pos + 4;
            break;

        case ICMP_TYPE_TIMESTAMP_REQUEST:
        case ICMP_TYPE_TIMESTAMP_REPLY:
            if (input->size < 12) {
                free(pkt);
                return 0;
            }

            pkt->data.timestamp.identifier = (pos[0] << 8) | pos[1];
            pkt->data.timestamp.sequence_number = (pos[2] << 8) | pos[3];
            pkt->data.timestamp.timestamp = (pos[4] << 24) | (pos[5] << 16) | (pos[6] << 8) | pos[7];

            pkt->rest = pos + 8;
            break;

        default:
            free(pkt);
            return 0;
    }

    *packet = pkt;

    return 1;
}

void init_input(input_t* input, uint8_t* data, size_t size) {
    input->data = data;
    input->size = size;
}

void free_input(input_t* input) {
}

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Failed to open file: %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    rewind(file);

    uint8_t* data = malloc(size);
    if (!data) {
        printf("Failed to allocate memory\n");
        fclose(file);
        return 1;
    }

    size_t read_size = fread(data, 1, size, file);
    if (read_size != size) {
        printf("Failed to read file\n");
        free(data);
        fclose(file);
        return 1;
    }

    fclose(file);

    input_t input;
    init_input(&input, data, size);

    icmp_packet_t* packet;
    if (icmp_parse(&input, NULL, &packet)) {
        printf("Parsed ICMP packet successfully\n");
        printf("Type: %u\n", packet->type);
        printf("Code: %u\n", packet->code);
        printf("Checksum: %u\n", packet->checksum);

        switch (packet->type) {
            case ICMP_TYPE_ECHO_REQUEST:
            case ICMP_TYPE_ECHO_REPLY:
                printf("Identifier: %u\n", packet->data.echo.identifier);
                printf("Sequence Number: %u\n", packet->data.echo.sequence_number);
                break;

            case ICMP_TYPE_REDIRECT:
                printf("Gateway Address: %u\n", packet->data.gateway_address);
                break;

            case ICMP_TYPE_TIME_EXCEEDED:
            case ICMP_TYPE_PARAMETER_PROBLEM:
                printf("Unused: %u\n", packet->data.unused);
                break;

            case ICMP_TYPE_TIMESTAMP_REQUEST:
            case ICMP_TYPE_TIMESTAMP_REPLY:
                printf("Identifier: %u\n", packet->data.timestamp.identifier);
                printf("Sequence Number: %u\n", packet->data.timestamp.sequence_number);
                printf("Timestamp: %u\n", packet->data.timestamp.timestamp);
                break;
        }

        free(packet);
    } else {
        printf("Failed to parse ICMP packet\n");
    }

    free(data);

    return 0;
}