#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    ICMP_ECHO_REPLY = 0,
    ICMP_DESTINATION_UNREACHABLE = 3,
    ICMP_SOURCE_QUENCH = 4,
    ICMP_REDIRECT = 5,
    ICMP_ECHO_REQUEST = 8,
    ICMP_ROUTER_ADVERTISEMENT = 9,
    ICMP_ROUTER_SOLICITATION = 10,
    ICMP_TIME_EXCEEDED = 11,
    ICMP_PARAMETER_PROBLEM = 12,
    ICMP_TIMESTAMP_REQUEST = 13,
    ICMP_TIMESTAMP_REPLY = 14,
    ICMP_INFORMATION_REQUEST = 15,
    ICMP_INFORMATION_REPLY = 16
} icmp_type_t;

typedef enum {
    ICMP_DESTINATION_UNREACHABLE_NETWORK_UNREACHABLE = 0,
    ICMP_DESTINATION_UNREACHABLE_HOST_UNREACHABLE = 1,
    ICMP_DESTINATION_UNREACHABLE_PROTOCOL_UNREACHABLE = 2,
    ICMP_DESTINATION_UNREACHABLE_PORT_UNREACHABLE = 3,
    ICMP_DESTINATION_UNREACHABLE_FRAGMENTATION_NEEDED = 4,
    ICMP_DESTINATION_UNREACHABLE_SOURCE_ROUTE_FAILED = 5,
    ICMP_DESTINATION_UNREACHABLE_DESTINATION_NETWORK_UNKNOWN = 6,
    ICMP_DESTINATION_UNREACHABLE_DESTINATION_HOST_UNKNOWN = 7,
    ICMP_DESTINATION_UNREACHABLE_SOURCE_HOST_ISOLATED = 8,
    ICMP_DESTINATION_UNREACHABLE_COMMUNICATION_WITH_DESTINATION_NETWORK_IS_ADMINISTRATIVELY_PROHIBITED = 9,
    ICMP_DESTINATION_UNREACHABLE_COMMUNICATION_WITH_DESTINATION_HOST_IS_ADMINISTRATIVELY_PROHIBITED = 10,
    ICMP_DESTINATION_UNREACHABLE_NETWORK_UNREACHABLE_FOR_TYPE_OF_SERVICE = 11,
    ICMP_DESTINATION_UNREACHABLE_HOST_UNREACHABLE_FOR_TYPE_OF_SERVICE = 12
} icmp_destination_unreachable_code_t;

typedef enum {
    ICMP_REDIRECT_REDIRECT_DATAGRAM_FOR_THE_NETWORK = 0,
    ICMP_REDIRECT_REDIRECT_DATAGRAM_FOR_THE_HOST = 1,
    ICMP_REDIRECT_REDIRECT_DATAGRAM_FOR_THE_TYPE_OF_SERVICE_AND_NETWORK = 2,
    ICMP_REDIRECT_REDIRECT_DATAGRAM_FOR_THE_TYPE_OF_SERVICE_AND_HOST = 3
} icmp_redirect_code_t;

typedef enum {
    ICMP_TIME_EXCEEDED_TIME_TO_LIVE_EXCEEDED_IN_TRANSIT = 0,
    ICMP_TIME_EXCEEDED_FRAGMENT_REASSEMBLY_TIME_EXCEEDED = 1
} icmp_time_exceeded_code_t;

typedef enum {
    ICMP_PARAMETER_PROBLEM_POINTER_INDICATES_THE_ERROR = 0,
    ICMP_PARAMETER_PROBLEM_MISSING_A_REQUIRED_OPTION = 1,
    ICMP_PARAMETER_PROBLEM_BAD_LENGTH = 2
} icmp_parameter_problem_code_t;

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
    uint32_t receive_timestamp;
    uint32_t transmit_timestamp;
} icmp_timestamp_reply_t;

typedef struct {
    icmp_header_t header;
    uint32_t identifier;
    uint32_t sequence_number;
} icmp_echo_t;

typedef struct {
    icmp_header_t header;
    uint32_t gateway_address;
} icmp_redirect_t;

typedef struct {
    icmp_header_t header;
    uint32_t unused;
    uint32_t mtu;
} icmp_destination_unreachable_t;

typedef struct {
    icmp_header_t header;
    uint32_t unused;
} icmp_source_quench_t;

typedef struct {
    icmp_header_t header;
    uint8_t pointer;
} icmp_parameter_problem_t;

typedef struct {
    icmp_header_t header;
    uint8_t pointer;
} icmp_time_exceeded_t;

typedef struct {
    icmp_header_t header;
} icmp_information_request_t;

typedef struct {
    icmp_header_t header;
} icmp_information_reply_t;

typedef struct {
    icmp_header_t header;
    uint32_t address;
} icmp_router_advertisement_t;

typedef struct {
    icmp_header_t header;
} icmp_router_solicitation_t;

void* icmp_parser(void* ctx) {
    void* type = h_uint8();
    void* code = h_uint8();
    void* checksum = h_uint16();
    void* identifier = h_uint16();
    void* sequence_number = h_uint16();
    void* timestamp = h_uint32();
    void* receive_timestamp = h_uint32();
    void* transmit_timestamp = h_uint32();
    void* gateway_address = h_uint32();
    void* unused = h_uint32();
    void* mtu = h_uint32();
    void* pointer = h_uint8();
    void* address = h_uint32();

    void* icmp_echo_reply = h_struct(
        type, code, checksum, identifier, sequence_number,
        h_action(h_ok, NULL, NULL)
    );

    void* icmp_destination_unreachable = h_struct(
        type, code, checksum, unused, mtu,
        h_action(h_ok, NULL, NULL)
    );

    void* icmp_source_quench = h_struct(
        type, code, checksum, unused,
        h_action(h_ok, NULL, NULL)
    );

    void* icmp_redirect = h_struct(
        type, code, checksum, gateway_address,
        h_action(h_ok, NULL, NULL)
    );

    void* icmp_echo_request = h_struct(
        type, code, checksum, identifier, sequence_number,
        h_action(h_ok, NULL, NULL)
    );

    void* icmp_router_advertisement = h_struct(
        type, code, checksum, address,
        h_action(h_ok, NULL, NULL)
    );

    void* icmp_router_solicitation = h_struct(
        type, code, checksum,
        h_action(h_ok, NULL, NULL)
    );

    void* icmp_time_exceeded = h_struct(
        type, code, checksum, pointer,
        h_action(h_ok, NULL, NULL)
    );

    void* icmp_parameter_problem = h_struct(
        type, code, checksum, pointer,
        h_action(h_ok, NULL, NULL)
    );

    void* icmp_timestamp_request = h_struct(
        type, code, checksum, identifier, sequence_number,
        h_action(h_ok, NULL, NULL)
    );

    void* icmp_timestamp_reply = h_struct(
        type, code, checksum, identifier, sequence_number, timestamp, receive_timestamp, transmit_timestamp,
        h_action(h_ok, NULL, NULL)
    );

    void* icmp_information_request = h_struct(
        type, code, checksum,
        h_action(h_ok, NULL, NULL)
    );

    void* icmp_information_reply = h_struct(
        type, code, checksum,
        h_action(h_ok, NULL, NULL)
    );

    return h_choice(
        icmp_echo_reply,
        icmp_destination_unreachable,
        icmp_source_quench,
        icmp_redirect,
        icmp_echo_request,
        icmp_router_advertisement,
        icmp_router_solicitation,
        icmp_time_exceeded,
        icmp_parameter_problem,
        icmp_timestamp_request,
        icmp_timestamp_reply,
        icmp_information_request,
        icmp_information_reply
    );
}

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Failed to open file %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        printf("Failed to allocate memory\n");
        fclose(file);
        return 1;
    }

    size_t bytes_read = fread(buffer, 1, file_size, file);
    if (bytes_read != file_size) {
        printf("Failed to read file\n");
        free(buffer);
        fclose(file);
        return 1;
    }

    fclose(file);

    void* parser = icmp_parser(NULL);
    HParseResult* result = h_parse(parser, buffer, file_size);
    if (result->is_ok) {
        printf("Parsed ICMP message:\n");
        switch (((icmp_header_t*)result->value)->type) {
            case ICMP_ECHO_REPLY:
                printf("Type: Echo Reply\n");
                break;
            case ICMP_DESTINATION_UNREACHABLE:
                printf("Type: Destination Unreachable\n");
                break;
            case ICMP_SOURCE_QUENCH:
                printf("Type: Source Quench\n");
                break;
            case ICMP_REDIRECT:
                printf("Type: Redirect\n");
                break;
            case ICMP_ECHO_REQUEST:
                printf("Type: Echo Request\n");
                break;
            case ICMP_ROUTER_ADVERTISEMENT:
                printf("Type: Router Advertisement\n");
                break;
            case ICMP_ROUTER_SOLICITATION:
                printf("Type: Router Solicitation\n");
                break;
            case ICMP_TIME_EXCEEDED:
                printf("Type: Time Exceeded\n");
                break;
            case ICMP_PARAMETER_PROBLEM:
                printf("Type: Parameter Problem\n");
                break;
            case ICMP_TIMESTAMP_REQUEST:
                printf("Type: Timestamp Request\n");
                break;
            case ICMP_TIMESTAMP_REPLY:
                printf("Type: Timestamp Reply\n");
                break;
            case ICMP_INFORMATION_REQUEST:
                printf("Type: Information Request\n");
                break;
            case ICMP_INFORMATION_REPLY:
                printf("Type: Information Reply\n");
                break;
            default:
                printf("Unknown type\n");
                break;
        }
    } else {
        printf("Failed to parse ICMP message\n");
    }

    free(buffer);
    return 0;
}