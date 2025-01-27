#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

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

// Parser for DNS header
HParser *dns_header_parser() {
    return h_sequence(
        h_bits_u16(16, &endianness),  // ID
        h_bits_u16(16, &endianness),  // Flags
        h_bits_u16(16, &endianness),  // QDCOUNT
        h_bits_u16(16, &endianness),  // ANCOUNT
        h_bits_u16(16, &endianness),  // NSCOUNT
        h_bits_u16(16, &endianness),  // ARCOUNT
        NULL
    );
}

// Parser for DNS name (labels)
HParser *dns_name_parser() {
    return h_sequence(
        h_many1(h_choice(
            h_uint8(),  // Single label
            h_sequence(h_uint8(), h_uint8(), NULL),  // Pointer
            NULL
        )),
        h_uint8(),  // Null terminator
        NULL
    );
}

// Parser for DNS question
HParser *dns_question_parser() {
    return h_sequence(
        dns_name_parser(),  // QNAME
        h_bits_u16(16, &endianness),  // QTYPE
        h_bits_u16(16, &endianness),  // QCLASS
        NULL
    );
}

// Parser for DNS resource record
HParser *dns_rr_parser() {
    return h_sequence(
        dns_name_parser(),  // NAME
        h_bits_u16(16, &endianness),  // TYPE
        h_bits_u16(16, &endianness),  // CLASS
        h_bits_u32(32, &endianness),  // TTL
        h_bits_u16(16, &endianness),  // RDLENGTH
        h_length_value(h_bits_u16(16, &endianness), h_uint8()),  // RDATA
        NULL
    );
}

// Parser for DNS packet
HParser *dns_packet_parser() {
    return h_sequence(
        dns_header_parser(),  // Header
        h_many(dns_question_parser()),  // Questions
        h_many(dns_rr_parser()),  // Answers
        h_many(dns_rr_parser()),  // Authority records
        h_many(dns_rr_parser()),  // Additional records
        NULL
    );
}

int main() {
    // Example DNS packet data
    uint8_t dns_packet[] = {
        // Header
        0x12, 0x34,  // ID
        0x01, 0x00,  // Flags
        0x00, 0x01,  // QDCOUNT
        0x00, 0x00,  // ANCOUNT
        0x00, 0x00,  // NSCOUNT
        0x00, 0x00,  // ARCOUNT
        // Question
        0x03, 'w', 'w', 'w', 0x07, 'e', 'x', 'a', 'm', 'p', 'l', 'e', 0x03, 'c', 'o', 'm', 0x00,  // QNAME
        0x00, 0x01,  // QTYPE
        0x00, 0x01   // QCLASS
    };

    // Parse the DNS packet
    HParseResult *result = h_parse(dns_packet_parser(), dns_packet, sizeof(dns_packet));
    if (result) {
        printf("DNS packet parsed successfully!\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse DNS packet.\n");
    }

    return 0;
}