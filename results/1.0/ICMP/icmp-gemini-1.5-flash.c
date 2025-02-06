#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <netinet/ip.h>
#include <netinet/ip_icmp.h>
#include <arpa/inet.h>

// Helper function to calculate ICMP checksum
uint16_t checksum(uint16_t *addr, int len) {
    int count = len;
    register uint32_t sum = 0;
    while (count > 1) {
        sum += *addr++;
        count -= 2;
    }
    if (count > 0) {
        sum += *(uint8_t *)addr;
    }
    while (sum >> 16) {
        sum = (sum & 0xFFFF) + (sum >> 16);
    }
    return ~sum;
}

// Hammer parser combinators for ICMP fields
HParser icmp_type = h_uint8;
HParser icmp_code = h_uint8;
HParser icmp_checksum = h_uint16_be;
HParser icmp_identifier = h_uint16_be;
HParser icmp_sequence = h_uint16_be;
HParser icmp_data = h_bytes(0, 1024); // Adjust max data size as needed

HParser icmp_header = h_seq(
    h_field("type", &icmp_type),
    h_field("code", &icmp_code),
    h_field("checksum", &icmp_checksum),
    h_field("identifier", &icmp_identifier),
    h_field("sequence", &icmp_sequence),
    h_field("data", &icmp_data)
);

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

    HParseResult result = h_parse(&icmp_header, buffer, fsize);

    if (result.parsed_len > 0) { // Check if parsing was successful.  HParseResult doesn't have a boolean success field.
        printf("ICMP Header Parsed Successfully:\n");
        HValue* val = &result.value;
        printf("Type: %u\n", *(uint8_t *)val->fields[0].value);
        printf("Code: %u\n", *(uint8_t *)val->fields[1].value);
        printf("Checksum: 0x%04X\n", *(uint16_t *)val->fields[2].value);
        printf("Identifier: %u\n", *(uint16_t *)val->fields[3].value);
        printf("Sequence: %u\n", *(uint16_t *)val->fields[4].value);
        printf("Data Length: %zu bytes\n", val->fields[5].length);
        //Further processing of data field as needed.

    } else {
        fprintf(stderr, "ICMP Header Parsing Failed: %s\n", result.error);
    }

    free(buffer);
    return 0;
}
