#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <arpa/inet.h>

typedef struct {
    uint16_t hardware_type;
    uint16_t protocol_type;
    uint8_t hardware_addr_len;
    uint8_t protocol_addr_len;
    uint16_t opcode;
    uint8_t sender_hardware_addr[6];
    uint8_t sender_protocol_addr[4];
    uint8_t target_hardware_addr[6];
    uint8_t target_protocol_addr[4];
} arp_packet;

static hammer_parser_t* parse_uint16_t(void) {
    return hammer_map(hammer_uint16_t(), (hammer_map_f)ntohl);
}

static hammer_parser_t* parse_uint8_t(void) {
    return hammer_uint8_t();
}

static hammer_parser_t* parse_hardware_addr(size_t len) {
    return hammer_bytes(len);
}

static hammer_parser_t* parse_protocol_addr(size_t len) {
    return hammer_bytes(len);
}

static hammer_parser_t* parse_arp_packet(void) {
    return hammer_seq(
        parse_uint16_t(),
        parse_uint16_t(),
        parse_uint8_t(),
        parse_uint8_t(),
        parse_uint16_t(),
        parse_hardware_addr(6),
        parse_protocol_addr(4),
        parse_hardware_addr(6),
        parse_protocol_addr(4),
        NULL
    );
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE* fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t* buffer = (uint8_t*)malloc(fsize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }

    fread(buffer, 1, fsize, fp);
    fclose(fp);

    hammer_parser_t* parser = parse_arp_packet();
    hammer_result_t result = hammer_parse(parser, buffer, fsize);

    if (result.status == HAMMER_SUCCESS) {
        arp_packet* packet = (arp_packet*)result.value;
        printf("Hardware Type: %u\n", packet->hardware_type);
        printf("Protocol Type: %u\n", packet->protocol_type);
        printf("Hardware Addr Len: %u\n", packet->hardware_addr_len);
        printf("Protocol Addr Len: %u\n", packet->protocol_addr_len);
        printf("Opcode: %u\n", packet->opcode);
        free(result.value);
    } else {
        fprintf(stderr, "Parsing failed: %s\n", result.error);
    }

    free(buffer);
    hammer_free(parser);

    return 0;
}
