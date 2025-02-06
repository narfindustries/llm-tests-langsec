#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define ICMP_TYPE_ECHO_REPLY 0
#define ICMP_TYPE_DESTINATION_UNREACHABLE 3
#define ICMP_TYPE_SOURCE_QUENCH 4
#define ICMP_TYPE_REDIRECT 5
#define ICMP_TYPE_ECHO_REQUEST 8
#define ICMP_TYPE_TIME_EXCEEDED 11

#define ICMP_CODE_DESTINATION_UNREACHABLE_NETWORK_UNREACHABLE 0
#define ICMP_CODE_DESTINATION_UNREACHABLE_HOST_UNREACHABLE 1
#define ICMP_CODE_DESTINATION_UNREACHABLE_PROTOCOL_UNREACHABLE 2
#define ICMP_CODE_DESTINATION_UNREACHABLE_PORT_UNREACHABLE 3
#define ICMP_CODE_DESTINATION_UNREACHABLE_FRAGMENTATION_NEEDED 4
#define ICMP_CODE_DESTINATION_UNREACHABLE_SOURCE_ROUTE_FAILED 5

#define ICMP_CODE_REDIRECT_NETWORK 0
#define ICMP_CODE_REDIRECT_HOST 1
#define ICMP_CODE_REDIRECT_TYPE_OF_SERVICE_AND_NETWORK 2
#define ICMP_CODE_REDIRECT_TYPE_OF_SERVICE_AND_HOST 3

#define ICMP_CODE_TIME_EXCEEDED_TTL_EXCEEDED 0
#define ICMP_CODE_TIME_EXCEEDED_FRAGMENT_REASSEMBLY_TIME_EXCEEDED 1

typedef struct {
    uint8_t type;
    uint8_t code;
    uint16_t checksum;
    uint16_t identifier;
    uint16_t sequence_number;
} icmp_header_t;

typedef struct {
    icmp_header_t header;
    uint32_t unused;
    uint8_t internet_header_and_data[1024];
} icmp_message_t;

typedef struct {
    icmp_header_t header;
    uint32_t unused;
    uint8_t internet_header_and_data[1024];
    uint8_t data[1024];
} icmp_echo_message_t;

void print_icmp_message(void* input) {
    icmp_message_t* message = (icmp_message_t*)input;
    printf("ICMP Type: %u\n", message->header.type);
    printf("ICMP Code: %u\n", message->header.code);
    printf("ICMP Checksum: %u\n", message->header.checksum);
    printf("ICMP Identifier: %u\n", message->header.identifier);
    printf("ICMP Sequence Number: %u\n", message->header.sequence_number);
}

void print_icmp_echo_message(void* input) {
    icmp_echo_message_t* message = (icmp_echo_message_t*)input;
    printf("ICMP Type: %u\n", message->header.type);
    printf("ICMP Code: %u\n", message->header.code);
    printf("ICMP Checksum: %u\n", message->header.checksum);
    printf("ICMP Identifier: %u\n", message->header.identifier);
    printf("ICMP Sequence Number: %u\n", message->header.sequence_number);
    printf("ICMP Data: ");
    for (int i = 0; i < 1024; i++) {
        printf("%02x", message->data[i]);
    }
    printf("\n");
}

HParser* icmp_parser() {
    HParser* type = h_uint8();
    HParser* code = h_uint8();
    HParser* checksum = h_uint16();
    HParser* identifier = h_uint16();
    HParser* sequence_number = h_uint16();
    HParser* unused = h_uint32();
    HParser* internet_header_and_data = h_bytes(1024);
    HParser* data = h_bytes(1024);

    HParser* icmp_header = h_tup2(type, h_tup4(code, checksum, identifier, sequence_number));

    HParser* icmp_message = h_tup3(icmp_header, unused, internet_header_and_data);

    HParser* icmp_echo_message = h_tup4(icmp_header, unused, internet_header_and_data, data);

    HParser* icmp = h_choice(icmp_message, icmp_echo_message);

    return icmp;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser* parser = icmp_parser();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (h_parse_result_is_ok(result)) {
        printf("ICMP message parsed successfully\n");
    } else {
        printf("Error parsing ICMP message\n");
    }

    free(buffer);
    return 0;
}