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
} arp_header_t;

typedef struct {
    uint8_t hardware_addr[6];
    uint8_t protocol_addr[4];
} arp_address_t;

static HParser* parse_uint16_t(void) {
    return h_map(h_le_uint16(), (HMapFunc)uint16_t);
}

static HParser* parse_uint8_t(void) {
    return h_map(h_uint8(), (HMapFunc)uint8_t);
}

static HParser* parse_arp_header(void) {
    return h_sequence(
        parse_uint16_t(),
        parse_uint16_t(),
        parse_uint8_t(),
        parse_uint8_t(),
        parse_uint16_t(),
        h_new_struct(arp_header_t)
    );
}

static HParser* parse_mac_address(void) {
    return h_sequenceN(6, parse_uint8_t(), h_new_array(uint8_t, 6));
}

static HParser* parse_ipv4_address(void) {
    return h_sequenceN(4, parse_uint8_t(), h_new_array(uint8_t, 4));
}

static HParser* parse_arp_address(void) {
    return h_sequence(
        parse_mac_address(),
        parse_ipv4_address(),
        h_new_struct(arp_address_t)
    );
}

static HParser* parse_arp_packet(void) {
    return h_sequence(
        parse_arp_header(),
        parse_arp_address(),
        parse_arp_address(),
        h_new_struct(arp_header_t)
    );
}

int main(int argc, char *argv[]) {
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

    uint8_t *buffer = (uint8_t *)malloc(fsize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }

    fread(buffer, 1, fsize, fp);
    fclose(fp);

    HParser* parser = parse_arp_packet();
    HParseResult* result = h_parse(parser, buffer, fsize);

    if (result->status == H_PARSE_SUCCESS) {
        arp_header_t* header = (arp_header_t*)result->value;
        arp_address_t* sender = (arp_address_t*)((uint8_t*)result->value + sizeof(arp_header_t));
        arp_address_t* target = (arp_address_t*)((uint8_t*)result->value + sizeof(arp_header_t) + sizeof(arp_address_t));

        printf("ARP Header:\n");
        printf("  Hardware Type: 0x%04X\n", header->hardware_type);
        printf("  Protocol Type: 0x%04X\n", header->protocol_type);
        printf("  Hardware Addr Len: %u\n", header->hardware_addr_len);
        printf("  Protocol Addr Len: %u\n", header->protocol_addr_len);
        printf("  Opcode: %u\n", header->opcode);

        printf("Sender Address:\n");
        printf("  MAC: %02X:%02X:%02X:%02X:%02X:%02X\n", sender->hardware_addr[0], sender->hardware_addr[1], sender->hardware_addr[2], sender->hardware_addr[3], sender->hardware_addr[4], sender->hardware_addr[5]);
        printf("  IP: %u.%u.%u.%u\n", sender->protocol_addr[0], sender->protocol_addr[1], sender->protocol_addr[2], sender->protocol_addr[3]);

        printf("Target Address:\n");
        printf("  MAC: %02X:%02X:%02X:%02X:%02X:%02X\n", target->hardware_addr[0], target->hardware_addr[1], target->hardware_addr[2], target->hardware_addr[3], target->hardware_addr[4], target->hardware_addr[5]);
        printf("  IP: %u.%u.%u.%u\n", target->protocol_addr[0], target->protocol_addr[1], target->protocol_addr[2], target->protocol_addr[3]);

        h_free_result(result);
    } else {
        fprintf(stderr, "Parsing failed: %s\n", result->error);
        h_free_result(result);
    }

    free(buffer);
    return 0;
}