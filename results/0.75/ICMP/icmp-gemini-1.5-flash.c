#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    uint8_t type;
    uint8_t code;
    uint16_t checksum;
    uint16_t identifier;
    uint16_t sequence_number;
    uint8_t data[1024]; 
} icmp_header;

hammer_parser icmp_type = hammer_uint8;
hammer_parser icmp_code = hammer_uint8;
hammer_parser icmp_checksum = hammer_uint16_be;
hammer_parser icmp_identifier = hammer_uint16_be;
hammer_parser icmp_sequence_number = hammer_uint16_be;
hammer_parser icmp_data = hammer_bytes(1024);

hammer_parser icmp_parser = hammer_seq(hammer_tuple5(icmp_type, icmp_code, icmp_checksum, icmp_identifier, icmp_sequence_number), icmp_data);

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <icmp_binary_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fileSize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buffer = (uint8_t *)malloc(fileSize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }

    fread(buffer, 1, fileSize, fp);
    fclose(fp);

    hammer_result result = hammer_parse(icmp_parser, buffer, fileSize);

    if (result.success) {
        icmp_header *icmp = (icmp_header *)result.value;
        printf("ICMP Header:\n");
        printf("Type: %u\n", icmp->type);
        printf("Code: %u\n", icmp->code);
        printf("Checksum: 0x%04X\n", icmp->checksum);
        printf("Identifier: %u\n", icmp->identifier);
        printf("Sequence Number: %u\n", icmp->sequence_number);
        printf("Data Length: %zu bytes\n", (size_t)result.length - 8);
    } else {
        fprintf(stderr, "Error parsing ICMP header: %s\n", result.error);
    }

    free(buffer);
    return 0;
}
