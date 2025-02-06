#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
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
            uint8_t length;
            uint8_t mtu;
        } fragmentation;
        uint32_t raw_data;
    } payload;
} ICMPPacket;

static HParser* icmp_parser(void) {
    return h_sequence(
        h_uint8(),    // type
        h_uint8(),    // code
        h_uint16(),   // checksum
        h_choice(
            h_sequence(
                h_uint16(),  // identifier
                h_uint16(),  // sequence number
                h_end_p(),
                NULL
            ),
            h_sequence(
                h_uint32(),  // raw data or original datagram
                h_end_p(),
                NULL
            ),
            NULL
        ),
        NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <icmp_binary_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    size_t read_size = fread(buffer, 1, file_size, file);
    fclose(file);

    if (read_size != file_size) {
        perror("File read error");
        free(buffer);
        return 1;
    }

    HParser* parser = icmp_parser();
    HParseResult* result = h_parse(parser, buffer, read_size);

    if (result && result->ast) {
        ICMPPacket* packet = (ICMPPacket*)result->ast;
        printf("ICMP Packet Details:\n");
        printf("Type: %d\n", packet->type);
        printf("Code: %d\n", packet->code);
        printf("Checksum: 0x%04x\n", packet->checksum);
        
        h_parse_result_free(result);
    } else {
        printf("Parsing failed\n");
    }

    h_destroy(parser);
    free(buffer);
    return 0;
}