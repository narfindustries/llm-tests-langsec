#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    uint16_t id;
    uint8_t qr;
    uint8_t opcode;
    uint8_t aa;
    uint8_t tc;
    uint8_t rd;
    uint8_t ra;
    uint8_t z;
    uint8_t rcode;
    uint16_t qdcount;
    uint16_t ancount;
    uint16_t nscount;
    uint16_t arcount;
} DNSHeader;

static DNSHeader *parse_dns_header(const uint8_t *input, size_t input_len) {
    HParser *parser = h_sequence(
        h_bits(16, &((DNSHeader *)0)->id),
        h_bits(1, &((DNSHeader *)0)->qr),
        h_bits(4, &((DNSHeader *)0)->opcode),
        h_bits(1, &((DNSHeader *)0)->aa),
        h_bits(1, &((DNSHeader *)0)->tc),
        h_bits(1, &((DNSHeader *)0)->rd),
        h_bits(1, &((DNSHeader *)0)->ra),
        h_bits(3, &((DNSHeader *)0)->z),
        h_bits(4, &((DNSHeader *)0)->rcode),
        h_bits(16, &((DNSHeader *)0)->qdcount),
        h_bits(16, &((DNSHeader *)0)->ancount),
        h_bits(16, &((DNSHeader *)0)->nscount),
        h_bits(16, &((DNSHeader *)0)->arcount),
        NULL
    );

    HParseResult *result = h_parse(parser, input, input_len);
    if (!result) return NULL;
    return (DNSHeader *)result->ast;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    DNSHeader *header = parse_dns_header(buffer, file_size);
    if (header) {
        printf("ID: %u\n", header->id);
        printf("QR: %u\n", header->qr);
        printf("OPCODE: %u\n", header->opcode);
        printf("AA: %u\n", header->aa);
        printf("TC: %u\n", header->tc);
        printf("RD: %u\n", header->rd);
        printf("RA: %u\n", header->ra);
        printf("Z: %u\n", header->z);
        printf("RCODE: %u\n", header->rcode);
        printf("QDCOUNT: %u\n", header->qdcount);
        printf("ANCOUNT: %u\n", header->ancount);
        printf("NSCOUNT: %u\n", header->nscount);
        printf("ARCOUNT: %u\n", header->arcount);
        free(header);
    } else {
        printf("Failed to parse DNS header.\n");
    }

    free(buffer);
    return 0;
}