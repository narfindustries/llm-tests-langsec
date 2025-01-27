#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// DNS Message Structure
typedef struct {
    uint16_t id;
    uint8_t qr, opcode, aa, tc, rd, ra, z, rcode;
    uint16_t qdcount, ancount, nscount, arcount;
} DNSHeader;

// Parser for DNS Header
static HParsedToken* parse_dns_header(void* p) {
    const HParseResult* result = h_parse(h_sequence(
        h_uint16(),   // ID
        h_bits(1),    // QR
        h_bits(4),    // OPCODE
        h_bits(1),    // AA
        h_bits(1),    // TC
        h_bits(1),    // RD
        h_bits(1),    // RA
        h_bits(3),    // Z
        h_bits(4),    // RCODE
        h_uint16(),   // QDCOUNT
        h_uint16(),   // ANCOUNT
        h_uint16(),   // NSCOUNT
        h_uint16()    // ARCOUNT
    ), p);

    if (!result || !result->ast) return NULL;

    DNSHeader* header = malloc(sizeof(DNSHeader));
    const HCountedArray* arr = result->ast;

    header->id = *(uint16_t*)arr->elements[0];
    header->qr = *(uint8_t*)arr->elements[1];
    header->opcode = *(uint8_t*)arr->elements[2];
    header->aa = *(uint8_t*)arr->elements[3];
    header->tc = *(uint8_t*)arr->elements[4];
    header->rd = *(uint8_t*)arr->elements[5];
    header->ra = *(uint8_t*)arr->elements[6];
    header->z = *(uint8_t*)arr->elements[7];
    header->rcode = *(uint8_t*)arr->elements[8];
    header->qdcount = *(uint16_t*)arr->elements[9];
    header->ancount = *(uint16_t*)arr->elements[10];
    header->nscount = *(uint16_t*)arr->elements[11];
    header->arcount = *(uint16_t*)arr->elements[12];

    return h_make_ast(header, sizeof(DNSHeader));
}

// DNS Name Parser
static HParsedToken* parse_dns_name(void* p) {
    HParser* label_parser = h_sequence(
        h_length_value(h_uint8(), h_many1(h_char_range('a', 'z'))),
        h_end_p()
    );
    return h_parse(label_parser, p);
}

// DNS Query Parser
static HParsedToken* parse_dns_query(void* p) {
    HParser* query_parser = h_sequence(
        parse_dns_name,  // QNAME
        h_uint16(),      // QTYPE
        h_uint16()       // QCLASS
    );
    return h_parse(query_parser, p);
}

// Main DNS Message Parser
static HParser* dns_parser() {
    return h_sequence(
        parse_dns_header,
        h_repeat_n(parse_dns_query, 1)  // At least one query
    );
}

int main() {
    HParser* parser = dns_parser();
    return 0;
}