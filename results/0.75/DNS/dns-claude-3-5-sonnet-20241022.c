#include <hammer/hammer.h>
#include <stdio.h>

static const HParser* dns_header();
static const HParser* dns_question();
static const HParser* dns_record();
static const HParser* dns_message();

static const HParser* dns_header() {
    return h_sequence(
        h_uint16(), // ID
        h_bits(16, false), // Flags
        h_uint16(), // QDCOUNT
        h_uint16(), // ANCOUNT
        h_uint16(), // NSCOUNT
        h_uint16(), // ARCOUNT
        NULL);
}

static const HParser* dns_name() {
    return h_many(h_sequence(
        h_uint8(),  // Length
        h_many_n(h_uint8(), 1), // Label
        NULL));
}

static const HParser* dns_question() {
    return h_sequence(
        dns_name(), // QNAME
        h_uint16(), // QTYPE
        h_uint16(), // QCLASS
        NULL);
}

static const HParser* dns_record() {
    return h_sequence(
        dns_name(), // NAME
        h_uint16(), // TYPE
        h_uint16(), // CLASS
        h_uint32(), // TTL
        h_length_value(h_uint16(), h_uint8()), // RDLENGTH + RDATA
        NULL);
}

static const HParser* dns_message() {
    return h_sequence(
        dns_header(),
        h_many(dns_question()),
        h_many(dns_record()), // Answer section
        h_many(dns_record()), // Authority section
        h_many(dns_record()), // Additional section
        NULL);
}

const HParser* init_parser() {
    return dns_message();
}