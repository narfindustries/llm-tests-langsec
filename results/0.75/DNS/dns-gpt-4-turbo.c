#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// DNS Header Struct
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

// Function to parse DNS header
HParser *parse_dns_header() {
    return h_sequence(
        h_uint16(),  // ID
        h_bits(1, false),  // QR
        h_bits(4, false),  // Opcode
        h_bits(1, false),  // AA
        h_bits(1, false),  // TC
        h_bits(1, false),  // RD
        h_bits(1, false),  // RA
        h_bits(3, false),  // Z
        h_bits(4, false),  // RCODE
        h_uint16(),  // QDCOUNT
        h_uint16(),  // ANCOUNT
        h_uint16(),  // NSCOUNT
        h_uint16(),  // ARCOUNT
        NULL
    );
}

// Function to parse DNS question
HParser *parse_dns_question() {
    return h_sequence(
        h_length_value(h_uint8(), h_uint8()),  // QNAME (length-prefixed)
        h_uint16(),  // QTYPE
        h_uint16(),  // QCLASS
        NULL
    );
}

// Function to parse DNS resource record
HParser *parse_dns_rr() {
    return h_sequence(
        h_length_value(h_uint8(), h_uint8()),  // NAME (length-prefixed)
        h_uint16(),  // TYPE
        h_uint16(),  // CLASS
        h_uint32(),  // TTL
        h_length_value(h_uint16(), h_uint8()),  // RDLENGTH and RDATA
        NULL
    );
}

// Function to parse the entire DNS message
HParser *parse_dns_message() {
    HParser *header = parse_dns_header();
    HParser *question = parse_dns_question();
    HParser *rr = parse_dns_rr();

    return h_sequence(
        header,
        h_many1(question),  // Questions
        h_many1(rr),  // Answers
        h_many1(rr),  // Authority
        h_many1(rr),  // Additional
        NULL
    );
}

// Main function that processes a DNS message from a file
int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <dns_message_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("File opening failed");
        return EXIT_FAILURE;
    }

    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buffer = malloc(size);
    if (!buffer) {
        fprintf(stderr, "Memory allocation failed\n");
        fclose(fp);
        return EXIT_FAILURE;
    }

    fread(buffer, sizeof(uint8_t), size, fp);
    fclose(fp);

    HParser *dns_parser = parse_dns_message();
    HParseResult *result = h_parse(dns_parser, buffer, size);
    if (result) {
        printf("DNS Message Parsed Successfully\n");
    } else {
        printf("Failed to parse DNS Message\n");
    }

    h_parse_result_free(result);
    free(buffer);
    return 0;
}