#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    uint16_t hardware_type;
    uint16_t protocol_type;
    uint8_t hardware_addr_len;
    uint8_t protocol_addr_len;
    uint16_t operation;
    HParsedToken* sender_hardware_addr;
    HParsedToken* sender_protocol_addr;
    HParsedToken* target_hardware_addr;
    HParsedToken* target_protocol_addr;
} ARPPacket;

HParser* arp_parser(void) {
    return h_sequence(
        h_uint16(),
        h_uint16(),
        h_uint8(),
        h_uint8(),
        h_uint16(),
        h_repeat_n(h_uint8(), 6),
        h_repeat_n(h_uint8(), 4),
        h_repeat_n(h_uint8(), 6),
        h_repeat_n(h_uint8(), 4),
        NULL
    );
}

void print_arp_packet(ARPPacket* packet) {
    printf("Hardware Type: %d\n", packet->hardware_type);
    printf("Protocol Type: 0x%04x\n", packet->protocol_type);
    printf("Hardware Address Length: %d\n", packet->hardware_addr_len);
    printf("Protocol Address Length: %d\n", packet->protocol_addr_len);
    printf("Operation: %d\n", packet->operation);

    printf("Sender Hardware Address: ");
    for (size_t i = 0; i < 6; i++) {
        printf("%02x%s", packet->sender_hardware_addr->seq->elements[i]->uint, 
               i < 5 ? ":" : "\n");
    }

    printf("Sender Protocol Address: ");
    for (size_t i = 0; i < 4; i++) {
        printf("%d%s", packet->sender_protocol_addr->seq->elements[i]->uint, 
               i < 3 ? "." : "\n");
    }

    printf("Target Hardware Address: ");
    for (size_t i = 0; i < 6; i++) {
        printf("%02x%s", packet->target_hardware_addr->seq->elements[i]->uint, 
               i < 5 ? ":" : "\n");
    }

    printf("Target Protocol Address: ");
    for (size_t i = 0; i < 4; i++) {
        printf("%d%s", packet->target_protocol_addr->seq->elements[i]->uint, 
               i < 3 ? "." : "\n");
    }
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

    HParser* parser = arp_parser();
    HParseResult* result = h_parse(parser, buffer, read_size);

    if (result && result->ast) {
        ARPPacket* packet = (ARPPacket*)result->ast;
        print_arp_packet(packet);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Parsing failed\n");
        free(buffer);
        h_destroy_parser(parser);
        return 1;
    }

    h_destroy_parser(parser);
    free(buffer);
    return 0;
}