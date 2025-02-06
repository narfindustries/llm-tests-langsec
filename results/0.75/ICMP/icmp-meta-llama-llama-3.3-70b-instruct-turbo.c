#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define ICMP_TYPE_ECHO_REPLY 0
#define ICMP_TYPE_DESTINATION_UNREACHABLE 3
#define ICMP_TYPE_SOURCE_QUENCH 4
#define ICMP_TYPE_REDIRECT 5
#define ICMP_TYPE_ECHO 8
#define ICMP_TYPE_ROUTER_ADVERTISEMENT 9
#define ICMP_TYPE_ROUTER_SOLICITATION 10
#define ICMP_TYPE_TIME_EXCEEDED 11
#define ICMP_TYPE_PARAMETER_PROBLEM 12
#define ICMP_TYPE_TIMESTAMP 13
#define ICMP_TYPE_TIMESTAMP_REPLY 14
#define ICMP_TYPE_INFORMATION_REQUEST 15
#define ICMP_TYPE_INFORMATION_REPLY 16

#define ICMP_CODE_DESTINATION_UNREACHABLE_NETWORK_UNREACHABLE 0
#define ICMP_CODE_DESTINATION_UNREACHABLE_HOST_UNREACHABLE 1
#define ICMP_CODE_DESTINATION_UNREACHABLE_PROTOCOL_UNREACHABLE 2
#define ICMP_CODE_DESTINATION_UNREACHABLE_PORT_UNREACHABLE 3
#define ICMP_CODE_DESTINATION_UNREACHABLE_FRAGMENTATION_NEEDED_AND_DONT_FRAGMENT_WAS_SET 4
#define ICMP_CODE_DESTINATION_UNREACHABLE_SOURCE_ROUTE_FAILED 5

#define ICMP_CODE_REDIRECT_REDIRECT_DATAGRAMS_FOR_THE_NETWORK 0
#define ICMP_CODE_REDIRECT_REDIRECT_DATAGRAMS_FOR_THE_HOST 1
#define ICMP_CODE_REDIRECT_REDIRECT_DATAGRAMS_FOR_THE_TYPE_OF_SERVICE_AND_NETWORK 2
#define ICMP_CODE_REDIRECT_REDIRECT_DATAGRAMS_FOR_THE_TYPE_OF_SERVICE_AND_HOST 3

#define ICMP_CODE_TIME_EXCEEDED_TIME_TO_LIVE_EXCEEDED_IN_TRANSIT 0
#define ICMP_CODE_TIME_EXCEEDED_FRAGMENT_REASSEMBLY_TIME_EXCEEDED 1

#define ICMP_CODE_PARAMETER_PROBLEM_POINTER_INDICATES_THE_ERROR 0
#define ICMP_CODE_PARAMETER_PROBLEM_MISSING_A_REQUIRED_OPTION 1

typedef struct {
    uint8_t type;
    uint8_t code;
    uint16_t checksum;
    uint16_t identifier;
    uint16_t sequence_number;
} icmp_header_t;

typedef struct {
    icmp_header_t header;
    uint32_t timestamp;
} icmp_timestamp_t;

typedef struct {
    icmp_header_t header;
    uint32_t timestamp;
    uint32_t timestamp_reply;
} icmp_timestamp_reply_t;

typedef struct {
    icmp_header_t header;
    uint32_t information_request;
} icmp_information_request_t;

typedef struct {
    icmp_header_t header;
    uint32_t information_reply;
} icmp_information_reply_t;

typedef struct {
    icmp_header_t header;
    uint8_t unused[4];
    uint32_t gateway_address;
} icmp_redirect_t;

typedef struct {
    icmp_header_t header;
    uint8_t unused[4];
    uint32_t router_address;
} icmp_router_advertisement_t;

typedef struct {
    icmp_header_t header;
    uint8_t unused[4];
    uint32_t router_address;
} icmp_router_solicitation_t;

typedef struct {
    icmp_header_t header;
    uint32_t originating_timestamp;
    uint32_t receive_timestamp;
    uint32_t transmit_timestamp;
} icmp_timestamp_transmit_t;

typedef struct {
    icmp_header_t header;
    uint8_t pointer;
    uint8_t unused[3];
} icmp_parameter_problem_t;

typedef struct {
    icmp_header_t header;
    uint8_t unused[4];
} icmp_destination_unreachable_t;

typedef struct {
    icmp_header_t header;
    uint8_t unused[4];
} icmp_source_quench_t;

typedef struct {
    icmp_header_t header;
    uint8_t unused[4];
} icmp_echo_t;

typedef struct {
    icmp_header_t header;
    uint8_t unused[4];
} icmp_echo_reply_t;

typedef struct {
    icmp_header_t header;
    uint8_t unused[4];
    uint32_t ip_header_and_first_64_bits_of_original_datagram;
} icmp_time_exceeded_t;

typedef struct {
    icmp_header_t header;
    uint8_t unused[4];
    uint32_t ip_header_and_first_64_bits_of_original_datagram;
} icmp_parameter_problem_with_ip_header_t;

void print_icmp_header(icmp_header_t *header) {
    printf("Type: %u\n", header->type);
    printf("Code: %u\n", header->code);
    printf("Checksum: %u\n", header->checksum);
    printf("Identifier: %u\n", header->identifier);
    printf("Sequence Number: %u\n", header->sequence_number);
}

void print_icmp_timestamp(icmp_timestamp_t *timestamp) {
    print_icmp_header(&timestamp->header);
    printf("Timestamp: %u\n", timestamp->timestamp);
}

void print_icmp_timestamp_reply(icmp_timestamp_reply_t *timestamp_reply) {
    print_icmp_header(&timestamp_reply->header);
    printf("Timestamp: %u\n", timestamp_reply->timestamp);
    printf("Timestamp Reply: %u\n", timestamp_reply->timestamp_reply);
}

void print_icmp_information_request(icmp_information_request_t *information_request) {
    print_icmp_header(&information_request->header);
    printf("Information Request: %u\n", information_request->information_request);
}

void print_icmp_information_reply(icmp_information_reply_t *information_reply) {
    print_icmp_header(&information_reply->header);
    printf("Information Reply: %u\n", information_reply->information_reply);
}

void print_icmp_redirect(icmp_redirect_t *redirect) {
    print_icmp_header(&redirect->header);
    printf("Gateway Address: %u\n", redirect->gateway_address);
}

void print_icmp_router_advertisement(icmp_router_advertisement_t *router_advertisement) {
    print_icmp_header(&router_advertisement->header);
    printf("Router Address: %u\n", router_advertisement->router_address);
}

void print_icmp_router_solicitation(icmp_router_solicitation_t *router_solicitation) {
    print_icmp_header(&router_solicitation->header);
    printf("Router Address: %u\n", router_solicitation->router_address);
}

void print_icmp_timestamp_transmit(icmp_timestamp_transmit_t *timestamp_transmit) {
    print_icmp_header(&timestamp_transmit->header);
    printf("Originating Timestamp: %u\n", timestamp_transmit->originating_timestamp);
    printf("Receive Timestamp: %u\n", timestamp_transmit->receive_timestamp);
    printf("Transmit Timestamp: %u\n", timestamp_transmit->transmit_timestamp);
}

void print_icmp_parameter_problem(icmp_parameter_problem_t *parameter_problem) {
    print_icmp_header(&parameter_problem->header);
    printf("Pointer: %u\n", parameter_problem->pointer);
}

void print_icmp_destination_unreachable(icmp_destination_unreachable_t *destination_unreachable) {
    print_icmp_header(&destination_unreachable->header);
}

void print_icmp_source_quench(icmp_source_quench_t *source_quench) {
    print_icmp_header(&source_quench->header);
}

void print_icmp_echo(icmp_echo_t *echo) {
    print_icmp_header(&echo->header);
}

void print_icmp_echo_reply(icmp_echo_reply_t *echo_reply) {
    print_icmp_header(&echo_reply->header);
}

void print_icmp_time_exceeded(icmp_time_exceeded_t *time_exceeded) {
    print_icmp_header(&time_exceeded->header);
    printf("IP Header and First 64 Bits of Original Datagram: %u\n", time_exceeded->ip_header_and_first_64_bits_of_original_datagram);
}

void print_icmp_parameter_problem_with_ip_header(icmp_parameter_problem_with_ip_header_t *parameter_problem_with_ip_header) {
    print_icmp_header(&parameter_problem_with_ip_header->header);
    printf("IP Header and First 64 Bits of Original Datagram: %u\n", parameter_problem_with_ip_header->ip_header_and_first_64_bits_of_original_datagram);
}

int main(int argc, char **argv) {
    if (argc != 2) {
        printf("Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        printf("Could not open file %s\n", argv[1]);
        return 1;
    }

    uint8_t buffer[1024];
    size_t bytes_read = fread(buffer, 1, 1024, file);

    if (bytes_read < sizeof(icmp_header_t)) {
        printf("Invalid ICMP packet\n");
        return 1;
    }

    icmp_header_t *header = (icmp_header_t *)buffer;

    switch (header->type) {
        case ICMP_TYPE_ECHO_REPLY:
            if (bytes_read < sizeof(icmp_echo_reply_t)) {
                printf("Invalid ICMP echo reply packet\n");
                return 1;
            }
            print_icmp_echo_reply((icmp_echo_reply_t *)buffer);
            break;
        case ICMP_TYPE_DESTINATION_UNREACHABLE:
            if (bytes_read < sizeof(icmp_destination_unreachable_t)) {
                printf("Invalid ICMP destination unreachable packet\n");
                return 1;
            }
            print_icmp_destination_unreachable((icmp_destination_unreachable_t *)buffer);
            break;
        case ICMP_TYPE_SOURCE_QUENCH:
            if (bytes_read < sizeof(icmp_source_quench_t)) {
                printf("Invalid ICMP source quench packet\n");
                return 1;
            }
            print_icmp_source_quench((icmp_source_quench_t *)buffer);
            break;
        case ICMP_TYPE_REDIRECT:
            if (bytes_read < sizeof(icmp_redirect_t)) {
                printf("Invalid ICMP redirect packet\n");
                return 1;
            }
            print_icmp_redirect((icmp_redirect_t *)buffer);
            break;
        case ICMP_TYPE_ECHO:
            if (bytes_read < sizeof(icmp_echo_t)) {
                printf("Invalid ICMP echo packet\n");
                return 1;
            }
            print_icmp_echo((icmp_echo_t *)buffer);
            break;
        case ICMP_TYPE_ROUTER_ADVERTISEMENT:
            if (bytes_read < sizeof(icmp_router_advertisement_t)) {
                printf("Invalid ICMP router advertisement packet\n");
                return 1;
            }
            print_icmp_router_advertisement((icmp_router_advertisement_t *)buffer);
            break;
        case ICMP_TYPE_ROUTER_SOLICITATION:
            if (bytes_read < sizeof(icmp_router_solicitation_t)) {
                printf("Invalid ICMP router solicitation packet\n");
                return 1;
            }
            print_icmp_router_solicitation((icmp_router_solicitation_t *)buffer);
            break;
        case ICMP_TYPE_TIME_EXCEEDED:
            if (bytes_read < sizeof(icmp_time_exceeded_t)) {
                printf("Invalid ICMP time exceeded packet\n");
                return 1;
            }
            print_icmp_time_exceeded((icmp_time_exceeded_t *)buffer);
            break;
        case ICMP_TYPE_PARAMETER_PROBLEM:
            if (bytes_read < sizeof(icmp_parameter_problem_t)) {
                printf("Invalid ICMP parameter problem packet\n");
                return 1;
            }
            print_icmp_parameter_problem((icmp_parameter_problem_t *)buffer);
            break;
        case ICMP_TYPE_TIMESTAMP:
            if (bytes_read < sizeof(icmp_timestamp_t)) {
                printf("Invalid ICMP timestamp packet\n");
                return 1;
            }
            print_icmp_timestamp((icmp_timestamp_t *)buffer);
            break;
        case ICMP_TYPE_TIMESTAMP_REPLY:
            if (bytes_read < sizeof(icmp_timestamp_reply_t)) {
                printf("Invalid ICMP timestamp reply packet\n");
                return 1;
            }
            print_icmp_timestamp_reply((icmp_timestamp_reply_t *)buffer);
            break;
        case ICMP_TYPE_INFORMATION_REQUEST:
            if (bytes_read < sizeof(icmp_information_request_t)) {
                printf("Invalid ICMP information request packet\n");
                return 1;
            }
            print_icmp_information_request((icmp_information_request_t *)buffer);
            break;
        case ICMP_TYPE_INFORMATION_REPLY:
            if (bytes_read < sizeof(icmp_information_reply_t)) {
                printf("Invalid ICMP information reply packet\n");
                return 1;
            }
            print_icmp_information_reply((icmp_information_reply_t *)buffer);
            break;
        default:
            printf("Unknown ICMP packet type\n");
            return 1;
    }

    fclose(file);
    return 0;
}