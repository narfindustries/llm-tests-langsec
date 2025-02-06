#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// ICMP Type values
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
        } echo;
        struct {
            uint8_t pointer;
            uint8_t unused[3];
        } param_problem;
        uint32_t gateway_addr;
        uint32_t unused;
    } rest_of_header;
    uint8_t* data;
} ICMP_Packet;

static HParsedToken* act_echo(const HParseResult* p, void* user_data) {
    return h_make_seqn(p);
}

static HParsedToken* act_param_problem(const HParseResult* p, void* user_data) {
    return h_make_seqn(p);
}

static HParsedToken* act_redirect(const HParseResult* p, void* user_data) {
    return h_make_seqn(p);
}

static HParsedToken* act_default(const HParseResult* p, void* user_data) {
    return h_make_seqn(p);
}

static HParser* create_icmp_parser(void) {
    // Basic field parsers
    HParser* type = h_uint8();
    HParser* code = h_uint8();
    HParser* checksum = h_uint16();
    
    // Rest of header parsers
    HParser* identifier = h_uint16();
    HParser* sequence = h_uint16();
    HParser* pointer = h_uint8();
    HParser* unused_24bits = h_repeat_n(h_uint8(), 3);
    HParser* gateway_addr = h_uint32();
    HParser* unused_32bits = h_uint32();

    // Data section parser (variable length)
    HParser* data = h_many(h_uint8());

    // Echo/Echo Reply format
    HParser* echo_format = h_sequence(identifier, sequence, data, NULL);

    // Parameter Problem format
    HParser* param_problem_format = h_sequence(pointer, unused_24bits, data, NULL);

    // Redirect format
    HParser* redirect_format = h_sequence(gateway_addr, data, NULL);

    // Default format (with unused 32 bits)
    HParser* default_format = h_sequence(unused_32bits, data, NULL);

    // Complete ICMP parser with proper action functions
    return h_sequence(type, code, checksum,
        h_choice(h_action(h_sequence(h_ch(ECHO_REPLY), echo_format, NULL), act_echo, NULL),
                h_action(h_sequence(h_ch(ECHO_REQUEST), echo_format, NULL), act_echo, NULL),
                h_action(h_sequence(h_ch(PARAM_PROBLEM), param_problem_format, NULL), act_param_problem, NULL),
                h_action(h_sequence(h_ch(REDIRECT), redirect_format, NULL), act_redirect, NULL),
                h_action(default_format, act_default, NULL),
                NULL),
        NULL);
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* input = malloc(size);
    if (!input) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if (fread(input, 1, size, file) != size) {
        perror("Failed to read file");
        free(input);
        fclose(file);
        return 1;
    }

    HParser* icmp_parser = create_icmp_parser();
    HParseResult* result = h_parse(icmp_parser, input, size);

    if (result) {
        printf("Successfully parsed ICMP packet\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse ICMP packet\n");
    }

    free(input);
    fclose(file);
    return 0;
}