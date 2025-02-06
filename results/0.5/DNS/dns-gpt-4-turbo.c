#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define DNS message structure constants
#define QR_MASK 0x8000
#define OPCODE_MASK 0x7800
#define AA_MASK 0x0400
#define TC_MASK 0x0200
#define RD_MASK 0x0100
#define RA_MASK 0x0080
#define Z_MASK 0x0070
#define RCODE_MASK 0x000F

// DNS types
#define TYPE_A 1
#define TYPE_NS 2
#define TYPE_CNAME 5
#define TYPE_SOA 6
#define TYPE_PTR 12
#define TYPE_MX 15
#define TYPE_TXT 16
#define TYPE_AAAA 28

// DNS classes
#define CLASS_IN 1

// Function prototypes
static HParser *dns_header();
static HParser *dns_question();
static HParser *dns_rr();
static HParser *dns_message();

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <dns_packet_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long fsize = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buf = malloc(fsize);
    if (!buf) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buf, 1, fsize, file);
    fclose(file);

    HParser *parser = dns_message();
    HParseResult *result = h_parse(parser, buf, fsize);
    if (result) {
        printf("DNS message parsed successfully.\n");
    } else {
        fprintf(stderr, "Failed to parse DNS message.\n");
    }

    h_parse_result_free(result);
    free(buf);
    h_parser_unref(parser);

    return 0;
}

static HParser *dns_header() {
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
        NULL);
}

static HParser *dns_question() {
    return h_sequence(
        h_indirect(), // QNAME
        h_uint16(),   // QTYPE
        h_uint16(),   // QCLASS
        NULL);
}

static HParser *dns_rr() {
    return h_sequence(
        h_indirect(), // NAME
        h_uint16(),   // TYPE
        h_uint16(),   // CLASS
        h_uint32(),   // TTL
        h_length_value(h_uint16(), h_bits(0, false)), // RDLENGTH and RDATA
        NULL);
}

static HParser *dns_message() {
    return h_sequence(
        dns_header(),
        h_many(dns_question()), // Questions
        h_many(dns_rr()),       // Answer RRs
        h_many(dns_rr()),       // Authority RRs
        h_many(dns_rr()),       // Additional RRs
        NULL);
}