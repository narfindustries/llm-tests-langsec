#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>
#include <hammer/glue.h>

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
            uint16_t next_hop_mtu;
        } fragmentation;
        struct {
            uint32_t gateway_address;
        } redirect;
        struct {
            uint32_t original_timestamp;
            uint32_t receive_timestamp;
            uint32_t transmit_timestamp;
        } timestamp;
        uint32_t raw_data;
    } payload;
} ICMPPacket;

HParser* icmp_parser() {
    HAllocator* allocator = h_default_allocator();
    HParser* parser = h_choice(allocator,
        h_sequence(allocator,
            h_uint8(),   // type
            h_uint8(),   // code
            h_uint16(),  // checksum
            h_choice(allocator,
                h_sequence(allocator, h_uint16(), h_uint16(), NULL),  // echo
                h_sequence(allocator, h_uint8(), h_repeat_n(h_uint8(), 3), NULL),  // parameter problem
                h_sequence(allocator, h_uint16(), h_uint16(), NULL),  // fragmentation
                h_uint32(),  // redirect/raw data
                h_sequence(allocator, h_uint32(), h_uint32(), h_uint32(), NULL)  // timestamp
            ),
            NULL
        )
    );
    return parser;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <icmp_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("Error reading file");
        free(buffer);
        fclose(file);
        return 1;
    }
    fclose(file);

    HParser *parser = icmp_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("ICMP Packet parsed successfully\n");
        h_parse_result_free(result);
    } else {
        printf("ICMP Packet parsing failed\n");
    }

    h_arena_free(parser->arena);
    free(buffer);
    return 0;
}