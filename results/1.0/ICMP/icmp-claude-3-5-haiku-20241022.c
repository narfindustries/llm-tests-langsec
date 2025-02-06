#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

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
            uint8_t pointer;
            uint8_t unused[3];
        } parameter_problem;
        struct {
            uint16_t unused;
            uint16_t mtu;
        } fragmentation;
        struct {
            uint32_t orig_timestamp;
            uint32_t recv_timestamp;
            uint32_t transmit_timestamp;
        } timestamp;
        uint32_t raw_data;
    } payload;
} ICMPPacket;

HParsedToken* action_icmp_parse(const HParseResult* p, void* user_data) {
    ICMPPacket* packet = malloc(sizeof(ICMPPacket));
    
    const HHashedSeq* seq = p->ast;
    packet->type = *(uint8_t*)seq->elements[0];
    packet->code = *(uint8_t*)seq->elements[1];
    packet->checksum = *(uint16_t*)seq->elements[2];
    
    HParsedToken* token = malloc(sizeof(HParsedToken));
    token->type = TT_USER;
    token->user = packet;
    
    return token;
}

void action_icmp_free(HParsedToken* token) {
    if (token && token->user) {
        free(token->user);
        free(token);
    }
}

HParser* icmp_parser() {
    return h_action(
        h_sequence(
            h_uint8(),   // type
            h_uint8(),   // code
            h_uint16(),  // checksum
            h_optional(
                h_sequence(
                    h_uint16(),  // identifier
                    h_uint16(),  // sequence number
                    NULL
                )
            ),
            NULL
        ),
        action_icmp_parse,
        action_icmp_free
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <icmp_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Cannot open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation failed");
        fclose(file);
        return 1;
    }

    size_t bytes_read = fread(buffer, 1, file_size, file);
    fclose(file);

    if (bytes_read != file_size) {
        perror("File read error");
        free(buffer);
        return 1;
    }

    HParser* parser = icmp_parser();
    HParseResult* result = h_parse(parser, buffer, bytes_read);

    if (result && result->ast) {
        printf("ICMP Packet parsed successfully\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Parsing failed\n");
    }

    h_parser_free(parser);
    free(buffer);
    return 0;
}