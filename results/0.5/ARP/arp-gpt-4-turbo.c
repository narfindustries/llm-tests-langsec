#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define constants for ARP
#define ETHERNET_HARDWARE_TYPE 1
#define IPV4_PROTOCOL_TYPE 0x0800
#define HW_ADDRESS_LENGTH 6
#define PROTOCOL_ADDRESS_LENGTH 4

// ARP operation codes
#define ARP_REQUEST 1
#define ARP_REPLY 2

// Function declarations
static void parse_arp(const uint8_t *input, size_t length);
static void print_arp_info(const HParsedToken *hw_addr, const HParsedToken *proto_addr);

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <ARP binary file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    const char *filename = argv[1];
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        fprintf(stderr, "Memory allocation failed\n");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    parse_arp(buffer, file_size);
    free(buffer);

    return EXIT_SUCCESS;
}

static void parse_arp(const uint8_t *input, size_t length) {
    HParser *uint16 = h_uint16();
    HParser *uint8 = h_uint8();
    HParser *hw_addr = h_bits(HW_ADDRESS_LENGTH * 8, false);
    HParser *proto_addr = h_bits(PROTOCOL_ADDRESS_LENGTH * 8, false);

    HParser *arp_parser = h_sequence(
        uint16, // Hardware type
        uint16, // Protocol type
        uint8,  // Hardware address length
        uint8,  // Protocol address length
        uint16, // Operation
        hw_addr, // Sender hardware address
        proto_addr, // Sender protocol address
        hw_addr, // Target hardware address
        proto_addr, // Target protocol address
        NULL
    );

    HParseResult *result = h_parse(arp_parser, input, length);
    if (result) {
        printf("ARP packet parsed successfully:\n");
        printf("Hardware Type: %d\n", *(uint16_t *)h_value_uint(result->ast->children[0]));
        printf("Protocol Type: 0x%04x\n", *(uint16_t *)h_value_uint(result->ast->children[1]));
        printf("Hardware Address Length: %d\n", *(uint8_t *)h_value_uint(result->ast->children[2]));
        printf("Protocol Address Length: %d\n", *(uint8_t *)h_value_uint(result->ast->children[3]));
        printf("Operation: %d\n", *(uint16_t *)h_value_uint(result->ast->children[4]));
        print_arp_info(result->ast->children[5], result->ast->children[6]);
        print_arp_info(result->ast->children[7], result->ast->children[8]);
    } else {
        fprintf(stderr, "Failed to parse ARP packet\n");
    }

    h_parse_result_free(result);
    h_free_parser(arp_parser);
    h_free_parser(uint16);
    h_free_parser(uint8);
    h_free_parser(hw_addr);
    h_free_parser(proto_addr);
}

static void print_arp_info(const HParsedToken *hw_addr, const HParsedToken *proto_addr) {
    printf("Hardware Address: ");
    for (size_t i = 0; i < HW_ADDRESS_LENGTH; i++) {
        printf("%02X", ((uint8_t *)h_value_bits(hw_addr))[i]);
        if (i < HW_ADDRESS_LENGTH - 1) printf(":");
    }
    printf("\n");

    printf("Protocol Address: ");
    for (size_t i = 0; i < PROTOCOL_ADDRESS_LENGTH; i++) {
        printf("%d", ((uint8_t *)h_value_bits(proto_addr))[i]);
        if (i < PROTOCOL_ADDRESS_LENGTH - 1) printf(".");
    }
    printf("\n");
}