#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>

typedef struct {
    uint8_t type;
    uint8_t code;
    uint16_t checksum;
    union {
        struct {
            uint16_t identifier;
            uint16_t sequence;
            uint8_t* data;
        } echo;
        struct {
            uint32_t unused;
            uint8_t* data;
        } unreach;
        uint32_t raw_rest;
    } payload;
} ICMP_Packet;

static HParser* create_icmp_parser(void) {
    HParser* type = h_uint8();
    HParser* code = h_uint8();
    HParser* checksum = h_uint16();
    
    // Echo request/reply structure
    HParser* echo_id = h_uint16();
    HParser* echo_seq = h_uint16();
    HParser* echo_data = h_many(h_uint8());
    HParser* echo_struct = h_sequence(echo_id, echo_seq, echo_data, NULL);
    
    // Destination unreachable structure
    HParser* unused = h_uint32();
    HParser* unreach_data = h_many(h_uint8());
    HParser* unreach_struct = h_sequence(unused, unreach_data, NULL);
    
    // Raw rest of packet
    HParser* raw_rest = h_uint32();
    
    // Choose parser based on type
    HParser* payload = h_choice(echo_struct, unreach_struct, raw_rest, NULL);
    
    return h_sequence(type, code, checksum, payload, NULL);
}

static void handle_icmp_packet(const HParseResult* result) {
    if (!result) {
        printf("Failed to parse ICMP packet\n");
        return;
    }
    
    // Extract parsed data here
    // This is a simplified version - you'd need to properly traverse the parse tree
    HParsedToken* token = result->ast;
    printf("ICMP Packet parsed successfully\n");
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    // Get file size
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    // Read entire file
    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("Failed to read file");
        free(buffer);
        fclose(file);
        return 1;
    }

    // Create and run parser
    HParser* icmp_parser = create_icmp_parser();
    HParseResult* result = h_parse(icmp_parser, buffer, file_size);
    
    handle_icmp_packet(result);

    // Cleanup
    h_parse_result_free(result);
    free(buffer);
    fclose(file);
    return 0;
}