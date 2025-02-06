#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// DNS Header structure
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

// DNS Question structure
typedef struct {
    HBytes *qname;
    uint16_t qtype;
    uint16_t qclass;
} DNSQuestion;

// DNS Resource Record structure
typedef struct {
    HBytes *name;
    uint16_t type;
    uint16_t class;
    uint32_t ttl;
    uint16_t rdlength;
    HBytes *rdata;
} DNSResourceRecord;

// Function to parse DNS Header
HParser *parse_dns_header() {
    return h_sequence(
        h_uint16(), // ID
        h_bits(1, false), // QR
        h_bits(4, false), // Opcode
        h_bits(1, false), // AA
        h_bits(1, false), // TC
        h_bits(1, false), // RD
        h_bits(1, false), // RA
        h_bits(3, false), // Z
        h_bits(4, false), // RCODE
        h_uint16(), // QDCOUNT
        h_uint16(), // ANCOUNT
        h_uint16(), // NSCOUNT
        h_uint16(), // ARCOUNT
        NULL
    );
}

// Function to parse DNS Question
HParser *parse_dns_question() {
    return h_sequence(
        h_length_value(h_uint8(), h_uint8()), // QNAME
        h_uint16(), // QTYPE
        h_uint16(), // QCLASS
        NULL
    );
}

// Function to parse DNS Resource Record
HParser *parse_dns_rr() {
    return h_sequence(
        h_length_value(h_uint8(), h_uint8()), // NAME
        h_uint16(), // TYPE
        h_uint16(), // CLASS
        h_uint32(), // TTL
        h_length_value(h_uint16(), h_uint8()), // RDLENGTH and RDATA
        NULL
    );
}

// Function to parse entire DNS message
HParser *parse_dns_message() {
    HParser *header = parse_dns_header();
    HParser *question = parse_dns_question();
    HParser *answer = parse_dns_rr();
    HParser *authority = parse_dns_rr();
    HParser *additional = parse_dns_rr();

    return h_sequence(
        header,
        h_many(question),
        h_many(answer),
        h_many(authority),
        h_many(additional),
        NULL
    );
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <dns_message_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(length);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, length, file);
    fclose(file);

    HParser *dns_parser = parse_dns_message();
    HParseResult *result = h_parse(dns_parser, data, length);
    if (result) {
        printf("DNS message parsed successfully.\n");
    } else {
        printf("Failed to parse DNS message.\n");
    }

    h_parse_result_free(result);
    free(data);

    return 0;
}