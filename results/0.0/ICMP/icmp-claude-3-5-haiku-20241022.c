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
        uint32_t raw_data;
    } payload;
} ICMPPacket;

HParser* icmp_parser() {
    return h_sequence(
        h_uint8(),   // type
        h_uint8(),   // code
        h_uint16(),  // checksum
        h_choice(
            h_sequence(
                h_uint16(), // identifier
                h_uint16(), // sequence number
                NULL
            ),
            h_sequence(
                h_uint8(),  // pointer
                h_repeat_n(h_uint8(), 3), // unused
                NULL
            ),
            h_sequence(
                h_uint16(), // unused
                h_uint16(), // MTU
                NULL
            ),
            h_uint32(), // raw data fallback
            NULL
        ),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
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
        ICMPPacket *packet = (ICMPPacket*)result->ast;
        printf("ICMP Packet:\n");
        printf("Type: %d\n", packet->type);
        printf("Code: %d\n", packet->code);
        printf("Checksum: 0x%04x\n", packet->checksum);
        
        free(buffer);
        h_parse_result_free(result);
        return 0;
    } else {
        fprintf(stderr, "Parsing failed\n");
        free(buffer);
        return 1;
    }
}