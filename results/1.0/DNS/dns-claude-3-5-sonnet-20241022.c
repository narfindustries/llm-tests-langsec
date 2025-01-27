#include <hammer/hammer.h>
#include <stdio.h>

static const HParser* init_dns_parser(void) {
    // Basic components
    H_RULE(label_char, h_not_in(".\0"));
    H_RULE(label, h_sequence(h_many1(label_char), h_ch('.')));
    H_RULE(domain_name, h_sequence(h_many1(label), h_end_p()));

    // Question section components
    H_RULE(qtype, h_int_range(uint16_t, 1, 65535));
    H_RULE(qclass, h_int_range(uint16_t, 1, 65535));
    H_RULE(question_section, h_sequence(domain_name, qtype, qclass, NULL));

    // Header components
    H_RULE(id, h_uint16());
    H_RULE(flags, h_uint16());
    H_RULE(qdcount, h_uint16());
    H_RULE(ancount, h_uint16());
    H_RULE(nscount, h_uint16());
    H_RULE(arcount, h_uint16());
    H_RULE(header, h_sequence(id, flags, qdcount, ancount, nscount, arcount, NULL));

    // Full DNS message
    H_RULE(dns_message, h_sequence(header, question_section, NULL));

    return dns_message;
}

// Main parser definition
HParser* dns_parser = NULL;

H_INSTALL_PARSER(dns_message, init_dns_parser);

int main(int argc, char** argv) {
    // Initialize the parser
    dns_parser = init_dns_parser();
    if (!dns_parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }

    // Parse input would go here
    return 0;
}