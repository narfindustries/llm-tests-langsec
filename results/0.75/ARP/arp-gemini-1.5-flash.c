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
    uint16_t opcode;
    uint8_t sender_hardware_addr[6]; 
    uint8_t sender_protocol_addr[4]; 
    uint8_t target_hardware_addr[6]; 
    uint8_t target_protocol_addr[4]; 

} arp_packet;

static hammer_parser_t* parse_uint16(void) {
    return hammer_uint16_t();
}

static hammer_parser_t* parse_uint8(void) {
    return hammer_uint8_t();
}

static hammer_parser_t* parse_mac_addr(void) {
    return hammer_bytes(6);
}

static hammer_parser_t* parse_ipv4_addr(void) {
    return hammer_bytes(4);
}

static hammer_parser_t* parse_arp_packet(void) {
    return hammer_seq(
        hammer_map(parse_uint16(), (hammer_map_func_t) & (uint16_t)),
        hammer_map(parse_uint16(), (hammer_map_func_t) & (uint16_t)),
        hammer_map(parse_uint8(), (hammer_map_func_t) & (uint8_t)),
        hammer_map(parse_uint8(), (hammer_map_func_t) & (uint8_t)),
        hammer_map(parse_uint16(), (hammer_map_func_t) & (uint16_t)),
        hammer_map(parse_mac_addr(), (hammer_map_func_t) memcpy),
        hammer_map(parse_ipv4_addr(), (hammer_map_func_t) memcpy),
        hammer_map(parse_mac_addr(), (hammer_map_func_t) memcpy),
        hammer_map(parse_ipv4_addr(), (hammer_map_func_t) memcpy),
        NULL
    );
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t* buffer = (uint8_t*)malloc(fsize);
    fread(buffer, 1, fsize, fp);
    fclose(fp);

    hammer_parser_t* parser = parse_arp_packet();
    hammer_result_t result = hammer_parse(parser, buffer, fsize);

    if (result.success) {
        arp_packet* packet = (arp_packet*)result.value;
        printf("Hardware Type: %u\n", packet->hardware_type);
        printf("Protocol Type: %u\n", packet->protocol_type);
        printf("Hardware Addr Len: %u\n", packet->hardware_addr_len);
        printf("Protocol Addr Len: %u\n", packet->protocol_addr_len);
        printf("Opcode: %u\n", packet->opcode);
        printf("Sender MAC: %02x:%02x:%02x:%02x:%02x:%02x\n",
               packet->sender_hardware_addr[0], packet->sender_hardware_addr[1],
               packet->sender_hardware_addr[2], packet->sender_hardware_addr[3],
               packet->sender_hardware_addr[4], packet->sender_hardware_addr[5]);
        printf("Sender IP: %u.%u.%u.%u\n",
               packet->sender_protocol_addr[0], packet->sender_protocol_addr[1],
               packet->sender_protocol_addr[2], packet->sender_protocol_addr[3]);
        printf("Target MAC: %02x:%02x:%02x:%02x:%02x:%02x\n",
               packet->target_hardware_addr[0], packet->target_hardware_addr[1],
               packet->target_hardware_addr[2], packet->target_hardware_addr[3],
               packet->target_hardware_addr[4], packet->target_hardware_addr[5]);
        printf("Target IP: %u.%u.%u.%u\n",
               packet->target_protocol_addr[0], packet->target_protocol_addr[1],
               packet->target_protocol_addr[2], packet->target_protocol_addr[3]);
        free(result.value);
    } else {
        fprintf(stderr, "Parsing failed at offset %zu: %s\n", result.offset, result.error);
    }

    free(buffer);
    hammer_free(parser);
    return 0;
}

The error persists because  `hammer_uint16_t`, `hammer_uint8_t`, `hammer_bytes`, `hammer_seq`, and `hammer_map` are not standard C functions.  They are functions from the Hammer parser combinator library.  You need to install the Hammer library and link it during compilation.  The compiler error indicates that it cannot find the definitions for these functions.  Without knowing your specific build environment, I cannot provide the exact compilation command. However, it will involve including the Hammer library header and linking against the Hammer library.  A typical compilation command might look something like this (adjust paths as needed):

gcc arp_parser.c -o arp_parser -I/path/to/hammer/include -L/path/to/hammer/lib -lhammer
