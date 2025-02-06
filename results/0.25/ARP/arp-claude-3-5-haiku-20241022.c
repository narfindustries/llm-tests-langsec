#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

typedef struct {
    uint16_t hardware_type;
    uint16_t protocol_type;
    uint8_t hardware_addr_len;
    uint8_t protocol_addr_len;
    uint16_t operation;
    const HParsedToken* sender_hardware_addr;
    const HParsedToken* sender_protocol_addr;
    const HParsedToken* target_hardware_addr;
    const HParsedToken* target_protocol_addr;
} ARPPacket;

static HParser* arp_parser(void) {
    return h_sequence(
        h_uint16(),
        h_uint16(),
        h_uint8(),
        h_uint8(),
        h_uint16(),
        h_repeat_n(h_uint8(), 6),
        h_repeat_n(h_uint8(), 4),
        h_repeat_n(h_uint8(), 6),
        h_repeat_n(h_uint8(), 4)
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <arp_packet_file>\n", argv[0]);
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
        perror("Memory allocation failed");
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

    HParser* parser = arp_parser();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result && result->ast) {
        const HParsedToken* const* elements = (const HParsedToken* const*)result->ast;
        printf("ARP Packet Details:\n");
        printf("Hardware Type: %d\n", elements[0]->uint);
        printf("Protocol Type: 0x%04x\n", elements[1]->uint);
        printf("Hardware Address Length: %d\n", elements[2]->uint);
        printf("Protocol Address Length: %d\n", elements[3]->uint);
        printf("Operation: %d\n", elements[4]->uint);
    } else {
        printf("Parsing failed\n");
    }

    h_parse_result_free(result);
    h_destroy_parser(parser);
    free(buffer);

    return 0;
}