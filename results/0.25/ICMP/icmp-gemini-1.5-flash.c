#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h> //Added for memcpy


typedef struct {
    uint8_t type;
    uint8_t code;
    uint16_t checksum;
    uint8_t data[];
} icmp_header;

hammer_parser icmp_parser() {
    return seq(h_uint8(), h_uint8(), h_uint16(), h_bytes(0));
}

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

    hammer_result result = hammer_parse(icmp_parser(), buffer, fsize);

    if (result.success) {
        icmp_header *icmp = (icmp_header *)malloc(result.length); // Allocate memory for the entire ICMP packet
        if (icmp == NULL) {
            perror("Memory allocation failed");
            free(buffer);
            return 1;
        }
        memcpy(icmp, result.value, result.length); //Copy the parsed data to the allocated memory.

        printf("ICMP Type: %u\n", icmp->type);
        printf("ICMP Code: %u\n", icmp->code);
        printf("ICMP Checksum: %u\n", icmp->checksum);
        printf("Data Length: %zu\n", result.length - sizeof(icmp_header));
        free(icmp);
    } else {
        fprintf(stderr, "Parsing failed at offset %zu: %s\n", result.offset, result.error);
    }

    free(buffer);
    return 0;
}
