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
        uint32_t gateway;
        struct {
            uint8_t pointer;
            uint8_t unused[3];
        } param;
        uint32_t unused;
    } rest_of_header;
    uint8_t* data;
} ICMP_Packet;

static HParser* create_icmp_parser(void) {
    // Basic fields
    HParser* type = h_uint8();
    HParser* code = h_uint8();
    HParser* checksum = h_uint16();
    
    // Rest of Header parsers
    HParser* identifier = h_uint16();
    HParser* sequence = h_uint16();
    HParser* gateway = h_uint32();
    HParser* pointer = h_uint8();
    HParser* unused_24bits = h_repeat_n(h_uint8(), 3);
    HParser* unused_32bits = h_uint32();
    
    // Echo/Echo Reply format
    HParser* echo_format = h_sequence(identifier, sequence, NULL);
    
    // Redirect format
    HParser* redirect_format = gateway;
    
    // Parameter Problem format
    HParser* param_format = h_sequence(pointer, unused_24bits, NULL);
    
    // Timestamp format
    HParser* timestamp_format = h_sequence(
        identifier, sequence,
        h_uint32(), // Original timestamp
        h_uint32(), // Receive timestamp
        h_uint32(), // Transmit timestamp
        NULL
    );
    
    // Variable data section
    HParser* data = h_many(h_uint8());
    
    // Complete ICMP packet
    return h_sequence(
        type,
        code,
        checksum,
        h_choice(echo_format, redirect_format, param_format, timestamp_format, unused_32bits, NULL),
        data,
        NULL
    );
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

    uint8_t* buffer = malloc(size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, size, file) != size) {
        perror("Failed to read file");
        free(buffer);
        fclose(file);
        return 1;
    }

    HParser* icmp_parser = create_icmp_parser();
    HParseResult* result = h_parse(icmp_parser, buffer, size);

    if (!result) {
        fprintf(stderr, "Failed to parse ICMP packet\n");
        free(buffer);
        fclose(file);
        return 1;
    }

    // Process the parsed result here
    // The result will be in result->ast

    h_parse_result_free(result);
    free(buffer);
    fclose(file);
    return 0;
}