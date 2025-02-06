#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define DNS message sections
#define QUERY 0
#define RESPONSE 1

// DNS record types
enum dns_type {
    DNS_A = 1,
    DNS_NS = 2,
    DNS_CNAME = 5,
    DNS_SOA = 6,
    DNS_PTR = 12,
    DNS_MX = 15,
    DNS_TXT = 16
};

// DNS class
#define DNS_IN 1  

// Function prototypes
HParser *build_dns_parser();

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <dns_binary_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    const char *filename = argv[1];
    FILE *fp = fopen(filename, "rb");
    if (!fp) {
        perror("File opening failed");
        return EXIT_FAILURE;
    }
    
    fseek(fp, 0, SEEK_END);
    size_t length = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    
    uint8_t *data = malloc(length);
    if (data) {
        fread(data, 1, length, fp);
    }
    fclose(fp);

    HParser *dns_parser = build_dns_parser();
    HParseResult *result = h_parse(dns_parser, data, length);

    if (result) {
        printf("DNS parsing successful.\n");
        // Here you would normally do something with the parse result.
        h_pprint(stdout, result->ast, 0, 4);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "DNS parsing failed.\n");
    }
    
    free(data);
    h_parser_unref(dns_parser);
    return 0;
}

HParser *build_dns_header() {
    return h_sequence(
        h_uint16(),                     // ID
        h_bits(1, false),               // QR
        h_bits(4, false),               // OPCODE
        h_bits(1, false),               // AA
        h_bits(1, false),               // TC
        h_bits(1, false),               // RD
        h_bits(1, false),               // RA
        h_bits(3, false),               // Z
        h_bits(4, false),               // RCODE
        h_uint16(),                     // QDCOUNT
        h_uint16(),                     – ANCOUNT
        h_uint16(),                     – NSCOUNT
        h_uint16(),                     – ARCOUNT
        NULL
    );
}

HParser *build_dns_question() {
    return h_sequence(
        h_length_value(h_uint8(), h_bytes(1)), // QNAME
        h_uint16(),                           // QTYPE
        h_uint16(),                           // QCLASS
        NULL
    );
}

HParser *build_dns_rr() {
    return h_sequence(
        h_length_value(h_uint8(), h_bytes(1)),   // NAME
        h_uint16(),                               // TYPE
        h_uint16(),                               – CLASS
        h_uint32(),                               – TTL
        h_length_value(h_uint16(), h_bytes(1)), – RDATA
        NULL
    );
}

HParser *build_dns_parser() {
    HParser *header = build_dns_header();
    HParser *question = build_dns_question();
    HParser *rr = build_dns_rr();
    
    return h_sequence(
        header,
        h_many(question),
        h_many(rr),
        h_many(rr),
        h_many(rr),
        NULL
    );
}