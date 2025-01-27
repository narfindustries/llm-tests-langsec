#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// DNS query type enumeration
typedef enum {
    DNS_TYPE_A = 1,
    DNS_TYPE_AAAA = 28,
    DNS_TYPE_CNAME = 5,
    DNS_TYPE_MX = 15
} DNSQueryType;

// DNS header structure
typedef struct {
    uint16_t id;
    uint8_t flags;
    uint8_t rcode : 4;
    uint8_t opcode : 4;
    uint16_t qdcount;
    uint16_t ancount;
    uint16_t nscount;
    uint16_t arcount;
} DNSHeader;

// DNS label parsing
static HParsedToken* parse_dns_label(void* p) {
    HParseResult* label_result = h_parse(h_many1(h_ch_range('a', 'z')), p);
    if (!label_result || !label_result->ast) return NULL;
    
    return label_result->ast;
}

// DNS query parser
static HParsedToken* parse_dns_query(void* p) {
    HParser* label_parser = h_action(h_many1(h_ch_range('a', 'z')), parse_dns_label, NULL);
    HParser* domain_parser = h_sepBy1(label_parser, h_ch('.'));
    HParser* type_parser = h_uint16();

    HParser* query_parser = h_sequence(domain_parser, type_parser, NULL);
    return h_parse(query_parser, p)->ast;
}

// DNS header parser
static HParser* create_dns_header_parser() {
    return h_sequence(
        h_uint16(),  // Transaction ID
        h_bits(16, false),  // Flags
        h_uint16(),  // Questions
        h_uint16(),  // Answer RRs
        h_uint16(),  // Authority RRs
        h_uint16(),  // Additional RRs
        NULL
    );
}

// DNS message parser
static HParser* create_dns_parser() {
    HParser* header_parser = create_dns_header_parser();
    HParser* query_parser = h_action(h_many1(h_ch_range('a', 'z')), parse_dns_query, NULL);

    return h_sequence(
        header_parser,
        query_parser,
        NULL
    );
}

int main() {
    // Initialize Hammer parser
    HParser* dns_parser = create_dns_parser();

    // Test input
    const char* test_input = "example.com\x00\x01";
    
    // Parse DNS message
    HParseResult* result = h_parse(dns_parser, test_input, strlen(test_input));

    if (result && result->ast) {
        printf("DNS message parsed successfully\n");
    } else {
        printf("DNS message parsing failed\n");
    }

    return 0;
}