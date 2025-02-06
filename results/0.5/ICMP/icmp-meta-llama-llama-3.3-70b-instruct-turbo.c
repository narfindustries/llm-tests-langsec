#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define BUFFER_SIZE 1024

typedef enum {
    ICMP_ECHO_REPLY = 0,
    ICMP_DESTINATION_UNREACHABLE = 3,
    ICMP_SOURCE_QUENCH = 4,
    ICMP_REDIRECT = 5,
    ICMP_ECHO_REQUEST = 8,
    ICMP_TIME_EXCEEDED = 11,
    ICMP_PARAMETER_PROBLEM = 12,
    ICMP_TIMESTAMP_REQUEST = 13,
    ICMP_TIMESTAMP_REPLY = 14,
    ICMP_INFORMATION_REQUEST = 15,
    ICMP_INFORMATION_REPLY = 16
} icmp_type;

typedef enum {
    ICMP_DESTINATION_UNREACHABLE_NETWORK_UNREACHABLE = 0,
    ICMP_DESTINATION_UNREACHABLE_HOST_UNREACHABLE = 1,
    ICMP_DESTINATION_UNREACHABLE_PROTOCOL_UNREACHABLE = 2,
    ICMP_DESTINATION_UNREACHABLE_PORT_UNREACHABLE = 3,
    ICMP_DESTINATION_UNREACHABLE_FRAGMENTATION_NEEDED_AND_DONT_FRAGMENT_WAS_SET = 4,
    ICMP_DESTINATION_UNREACHABLE_SOURCE_ROUTE_FAILED = 5
} icmp_destination_unreachable_code;

typedef enum {
    ICMP_REDIRECT_REDIRECT_DATAGRAM_FOR_THE_NETWORK = 0,
    ICMP_REDIRECT_REDIRECT_DATAGRAM_FOR_THE_HOST = 1,
    ICMP_REDIRECT_REDIRECT_DATAGRAM_FOR_THE_TYPE_OF_SERVICE_AND_NETWORK = 2,
    ICMP_REDIRECT_REDIRECT_DATAGRAM_FOR_THE_TYPE_OF_SERVICE_AND_HOST = 3
} icmp_redirect_code;

typedef enum {
    ICMP_TIME_EXCEEDED_TIME_TO_LIVE_EXCEEDED_IN_TRANSIT = 0,
    ICMP_TIME_EXCEEDED_FRAGMENT_REASSEMBLY_TIME_EXCEEDED = 1
} icmp_time_exceeded_code;

typedef enum {
    ICMP_PARAMETER_PROBLEM_POINTER_INDICATES_THE_ERROR = 0,
    ICMP_PARAMETER_PROBLEM_MISSING_A_REQUIRED_OPTION = 1,
    ICMP_PARAMETER_PROBLEM_BAD_LENGTH = 2
} icmp_parameter_problem_code;

typedef struct {
    uint8_t type;
    uint8_t code;
    uint16_t checksum;
    uint16_t identifier;
    uint16_t sequence_number;
} icmp_header;

typedef struct {
    icmp_header header;
    uint32_t gateway_internet_address;
} icmp_redirect;

typedef struct {
    icmp_header header;
    uint64_t timestamp;
} icmp_timestamp;

typedef struct {
    icmp_header header;
    uint8_t data[BUFFER_SIZE];
} icmp_echo;

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

    uint8_t buffer[BUFFER_SIZE];
    size_t bytes_read = fread(buffer, 1, BUFFER_SIZE, file);
    if (bytes_read < 8) {
        printf("Error reading file %s\n", argv[1]);
        return 1;
    }

    icmp_header* header = (icmp_header*)buffer;
    printf("Type: %d\n", header->type);
    printf("Code: %d\n", header->code);
    printf("Checksum: %d\n", header->checksum);
    printf("Identifier: %d\n", header->identifier);
    printf("Sequence Number: %d\n", header->sequence_number);

    if (header->type == ICMP_REDIRECT) {
        icmp_redirect* redirect = (icmp_redirect*)buffer;
        printf("Gateway Internet Address: %u\n", redirect->gateway_internet_address);
    } else if (header->type == ICMP_TIMESTAMP_REQUEST || header->type == ICMP_TIMESTAMP_REPLY) {
        icmp_timestamp* timestamp = (icmp_timestamp*)buffer;
        printf("Timestamp: %llu\n", timestamp->timestamp);
    } else if (header->type == ICMP_ECHO_REQUEST) {
        icmp_echo* echo = (icmp_echo*)buffer;
        printf("Data: ");
        for (int i = 0; i < BUFFER_SIZE; i++) {
            printf("%02x ", echo->data[i]);
        }
        printf("\n");
    }

    fclose(file);
    return 0;
}