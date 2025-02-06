#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define DNS message sections
#define QR_QUERY 0
#define QR_RESPONSE 1

// Opcode values
#define OPCODE_QUERY 0
#define OPCODE_IQUERY 1
#define OPCODE_STATUS 2

// RCODE values
#define RCODE_NO_ERROR 0
#define RCODE_FORMAT_ERROR 1
#define RCODE_SERVER_FAILURE 2
#define RCODE_NAME_ERROR 3
#define RCODE_NOT_IMPLEMENTED 4
#define RCODE_REFUSED 5

// DNS record types
#define TYPE_A 1
#define TYPE_NS 2
#define TYPE_CNAME 5
#define TYPE_SOA 6
#define TYPE_PTR 12
#define TYPE_MX 15
#define TYPE_TXT 16

// DNS record class
#define CLASS_IN 1

// Parsing functions
HParser *parse_uint16() {
    return h_uint16();
}

HParser *parse_uint32() {
    return h_uint32();
}

HParser *parse_label() {
    return h_length_value(h_uint8(), h_uint8());
}

HParser *parse_domain_name() {
    return h_many1(parse_label());
}

HParser *parse_header() {
    return h_sequence(
        parse_uint16(), // ID
        h_bits(1, false), // QR
        h_bits(4, false), // Opcode
        h_bits(1, false), // AA
        h_bits(1, false), // TC
        h_bits(1, false), // RD
        h_bits(1, false), // RA
        h_bits(3, false), // Z
        h_bits(4, false), // RCODE
        parse_uint16(), // QDCOUNT
        parse_uint16(), // ANCOUNT
        parse_uint16(), // NSCOUNT
        parse_uint16(), // ARCOUNT
        NULL
    );
}

HParser *parse_question() {
    return h_sequence(
        parse_domain_name(),
        parse_uint16(), // QTYPE
        parse_uint16(), // QCLASS
        NULL
    );
}

HParser *parse_rr() {
    return h_sequence(
        parse_domain_name(),
        parse_uint16(), // TYPE
        parse_uint16(), // CLASS
        parse_uint32(), // TTL
        h_length_value(parse_uint16(), h_indirect()), // RDLENGTH and RDATA
        NULL
    );
}

HParser *parse_message() {
    return h_sequence(
        parse_header(),
        h_many(parse_question()), // Questions
        h_many(parse_rr()), // Answer RRs
        h_many(parse_rr()), // Authority RRs
        h_many(parse_rr()), // Additional RRs
        NULL
    );
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <dns_message_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("File opening failed");
        return EXIT_FAILURE;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buffer = malloc(fsize);
    fread(buffer, 1, fsize, fp);
    fclose(fp);

    HParser *dns_parser = parse_message();
    HParseResult *result = h_parse(dns_parser, buffer, fsize);
    if (result) {
        printf("DNS message parsed successfully.\n");
    } else {
        printf("Failed to parse DNS message.\n");
    }

    h_parse_result_free(result);
    h_free_parser(dns_parser);
    free(buffer);

    return 0;
}