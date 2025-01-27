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
    char *qname;
    uint16_t qtype;
    uint16_t qclass;
} dns_question_t;

// Define the DNS resource record structure
typedef struct {
    char *name;
    uint16_t type;
    uint16_t class;
    uint32_t ttl;
    uint16_t rdlength;
    char *rdata;
} dns_rr_t;

// Parser for DNS header
HParser *dns_header_parser() {
    return h_sequence(
        h_bits(16, &dns_header_t, id),
        h_bits(16, &dns_header_t, flags),
        h_bits(16, &dns_header_t, qdcount),
        h_bits(16, &dns_header_t, ancount),
        h_bits(16, &dns_header_t, nscount),
        h_bits(16, &dns_header_t, arcount),
        NULL
    );
}

// Parser for DNS name (domain name)
HParser *dns_name_parser() {
    return h_sequence(
        h_length_value(h_uint8(), h_choice(h_char(), h_end_p())),
        NULL
    );
}

// Parser for DNS question
HParser *dns_question_parser() {
    return h_sequence(
        dns_name_parser(),
        h_bits(16, &dns_question_t, qtype),
        h_bits(16, &dns_question_t, qclass),
        NULL
    );
}

// Parser for DNS resource record
HParser *dns_rr_parser() {
    return h_sequence(
        dns_name_parser(),
        h_bits(16, &dns_rr_t, type),
        h_bits(16, &dns_rr_t, class),
        h_bits(32, &dns_rr_t, ttl),
        h_bits(16, &dns_rr_t, rdlength),
        h_length_value(h_uint16(), h_choice(h_char(), h_end_p())),
        NULL
    );
}

// Main DNS parser
HParser *dns_parser() {
    return h_sequence(
        dns_header_parser(),
        h_repeat_n(dns_question_parser(), h_uint16()),
        h_repeat_n(dns_rr_parser(), h_uint16()),
        NULL
    );
}

int main(int argc, char **argv) {
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

    uint8_t *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(dns_parser(), data, file_size);
    if (!result) {
        fprintf(stderr, "Failed to parse DNS packet\n");
        free(data);
        return 1;
    }

    // Process the parsed result here

    h_parse_result_free(result);
    free(data);

    return 0;
}