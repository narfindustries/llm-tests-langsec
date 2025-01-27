#include <hammer/hammer.h>
#include <stdio.h>

static const HParser* init_dns_parser() {
    // DNS header fields
    H_RULE(id, h_uint16());
    H_RULE(flags, h_uint16());
    H_RULE(qdcount, h_uint16());
    H_RULE(ancount, h_uint16());
    H_RULE(nscount, h_uint16());
    H_RULE(arcount, h_uint16());

    // DNS name components
    H_RULE(label_length, h_uint8());
    H_RULE(label_char, h_in_range(0x2D, 0x7A)); // '-' to 'z'
    H_RULE(label, h_length_value(label_length, h_many1(label_char)));
    H_RULE(name, h_many1(label));

    // Query section
    H_RULE(qtype, h_uint16());
    H_RULE(qclass, h_uint16());
    H_RULE(question, h_sequence(name, qtype, qclass, NULL));
    H_RULE(questions, h_repeat_n(question, 1)); // At least one question

    // Resource record
    H_RULE(ttl, h_uint32());
    H_RULE(rdlength, h_uint16());
    H_RULE(rdata, h_length_value(rdlength, h_uint8()));
    H_RULE(resource_record, h_sequence(name, qtype, qclass, ttl, rdata, NULL));
    H_RULE(answers, h_repeat_n(resource_record, 0));
    H_RULE(authorities, h_repeat_n(resource_record, 0));
    H_RULE(additionals, h_repeat_n(resource_record, 0));

    // Complete DNS message
    H_RULE(dns_message, h_sequence(id, flags, qdcount, ancount, nscount, arcount,
                                  questions, answers, authorities, additionals,
                                  NULL));

    return dns_message;
}

int main(int argc, char** argv) {
    H_PARSER(dns_parser, init_dns_parser());
    if (!dns_parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }
    return 0;
}