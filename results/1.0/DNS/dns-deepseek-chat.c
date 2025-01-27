#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define the DNS header structure
typedef struct {
    uint16_t id;
    uint16_t flags;
    uint16_t qdcount;
    uint16_t ancount;
    uint16_t nscount;
    uint16_t arcount;
} dns_header_t;

// Define the DNS question structure
typedef struct {
    uint8_t *qname;
    uint16_t qtype;
    uint16_t qclass;
} dns_question_t;

// Define the DNS resource record structure
typedef struct {
    uint8_t *name;
    uint16_t type;
    uint16_t class;
    uint32_t ttl;
    uint16_t rdlength;
    uint8_t *rdata;
} dns_rr_t;

// Define the DNS packet structure
typedef struct {
    dns_header_t header;
    dns_question_t *questions;
    dns_rr_t *answers;
    dns_rr_t *authorities;
    dns_rr_t *additionals;
} dns_packet_t;

// Parser for DNS header
HParser *dns_header_parser() {
    return h_sequence(
        h_bits_be(16, &dns_header_t, id),
        h_bits_be(16, &dns_header_t, flags),
        h_bits_be(16, &dns_header_t, qdcount),
        h_bits_be(16, &dns_header_t, ancount),
        h_bits_be(16, &dns_header_t, nscount),
        h_bits_be(16, &dns_header_t, arcount),
        NULL
    );
}

// Parser for DNS name (domain name)
HParser *dns_name_parser() {
    return h_sequence(
        h_length_value(h_uint8(), h_choice(
            h_ch_range(0x01, 0x3F), // Labels are 1-63 bytes
            NULL
        )),
        NULL
    );
}

// Parser for DNS question
HParser *dns_question_parser() {
    return h_sequence(
        dns_name_parser(),
        h_bits_be(16, &dns_question_t, qtype),
        h_bits_be(16, &dns_question_t, qclass),
        NULL
    );
}

// Parser for DNS resource record
HParser *dns_rr_parser() {
    return h_sequence(
        dns_name_parser(),
        h_bits_be(16, &dns_rr_t, type),
        h_bits_be(16, &dns_rr_t, class),
        h_bits_be(32, &dns_rr_t, ttl),
        h_bits_be(16, &dns_rr_t, rdlength),
        h_length_value(h_uint16(), h_many(h_uint8())), // rdata
        NULL
    );
}

// Parser for DNS packet
HParser *dns_packet_parser() {
    return h_sequence(
        dns_header_parser(),
        h_many(dns_question_parser()),
        h_many(dns_rr_parser()),
        h_many(dns_rr_parser()),
        h_many(dns_rr_parser()),
        NULL
    );
}

// Main function to parse DNS packet
int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <dns_packet_file>\n", argv[0]);
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

    HParser *parser = dns_packet_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (!result) {
        fprintf(stderr, "Failed to parse DNS packet\n");
        free(buffer);
        return 1;
    }

    // Access parsed data here (result->ast)
    // ...

    h_parse_result_free(result);
    free(buffer);

    return 0;
}