#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// ICMP Types
#define ECHO_REPLY 0
#define DEST_UNREACHABLE 3
#define SOURCE_QUENCH 4
#define REDIRECT 5
#define ECHO_REQUEST 8
#define TIME_EXCEEDED 11
#define PARAM_PROBLEM 12
#define TIMESTAMP 13
#define TIMESTAMP_REPLY 14
#define INFO_REQUEST 15
#define INFO_REPLY 16

typedef struct {
    uint8_t type;
    uint8_t code;
    uint16_t checksum;
    union {
        struct {
            uint16_t identifier;
            uint16_t sequence;
            uint8_t *data;
        } echo;
        struct {
            uint32_t unused;
            uint8_t *original_datagram;
        } dest_unreachable;
        struct {
            uint32_t unused;
            uint8_t *original_datagram;
        } source_quench;
        struct {
            uint32_t gateway_addr;
            uint8_t *original_datagram;
        } redirect;
        struct {
            uint32_t unused;
            uint8_t *original_datagram;
        } time_exceeded;
        struct {
            uint8_t pointer;
            uint8_t unused[3];
            uint8_t *original_datagram;
        } param_problem;
        struct {
            uint16_t identifier;
            uint16_t sequence;
            uint32_t originate_timestamp;
            uint32_t receive_timestamp;
            uint32_t transmit_timestamp;
        } timestamp;
        struct {
            uint16_t identifier;
            uint16_t sequence;
        } info;
    } data;
} icmp_message;

static HParser *common_header;
static HParser *echo_parser;
static HParser *dest_unreachable_parser;
static HParser *source_quench_parser;
static HParser *redirect_parser;
static HParser *time_exceeded_parser;
static HParser *param_problem_parser;
static HParser *timestamp_parser;
static HParser *info_parser;
static HParser *icmp_parser;

void init_parsers(void) {
    // Common header fields
    HParser *type = h_uint8();
    HParser *code = h_uint8();
    HParser *checksum = h_uint16();
    common_header = h_sequence(type, code, checksum, NULL);

    // Echo/Echo Reply message
    HParser *identifier = h_uint16();
    HParser *sequence = h_uint16();
    HParser *data = h_many(h_uint8());
    echo_parser = h_sequence(identifier, sequence, data, NULL);

    // Destination Unreachable message
    HParser *unused32 = h_uint32();
    HParser *original_datagram = h_many(h_uint8());
    dest_unreachable_parser = h_sequence(unused32, original_datagram, NULL);

    // Source Quench message
    source_quench_parser = h_sequence(unused32, original_datagram, NULL);

    // Redirect message
    HParser *gateway_addr = h_uint32();
    redirect_parser = h_sequence(gateway_addr, original_datagram, NULL);

    // Time Exceeded message
    time_exceeded_parser = h_sequence(unused32, original_datagram, NULL);

    // Parameter Problem message
    HParser *pointer = h_uint8();
    HParser *unused24 = h_repeat_n(h_uint8(), 3);
    param_problem_parser = h_sequence(pointer, unused24, original_datagram, NULL);

    // Timestamp message
    HParser *originate_timestamp = h_uint32();
    HParser *receive_timestamp = h_uint32();
    HParser *transmit_timestamp = h_uint32();
    timestamp_parser = h_sequence(identifier, sequence, originate_timestamp,
                                receive_timestamp, transmit_timestamp, NULL);

    // Information Request/Reply message
    info_parser = h_sequence(identifier, sequence, NULL);

    // Complete ICMP parser
    icmp_parser = h_sequence(common_header,
                           h_choice(echo_parser, dest_unreachable_parser,
                                  source_quench_parser, redirect_parser,
                                  time_exceeded_parser, param_problem_parser,
                                  timestamp_parser, info_parser, NULL),
                           NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open input file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t *input = malloc(size);
    if (!input) {
        perror("Failed to allocate memory");
        fclose(f);
        return 1;
    }

    if (fread(input, 1, size, f) != size) {
        perror("Failed to read input file");
        free(input);
        fclose(f);
        return 1;
    }

    init_parsers();

    HParseResult *result = h_parse(icmp_parser, input, size);
    if (!result) {
        fprintf(stderr, "Failed to parse ICMP message\n");
        free(input);
        fclose(f);
        return 1;
    }

    // Process the parsed result here
    // The parsed data is in result->ast

    h_parse_result_free(result);
    free(input);
    fclose(f);
    return 0;
}