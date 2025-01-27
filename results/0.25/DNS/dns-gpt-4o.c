#include <hammer/hammer.h>

HParser *dns_label() {
    return h_many1(h_choice(h_ch_range('a', 'z'), h_ch_range('A', 'Z'), h_ch_range('0', '9'), h_ch('-')));
}

HParser *dns_name() {
    HParser *label = dns_label();
    HParser *dot = h_ch('.');
    return h_many1(h_sequence(label, h_many(dot, label), NULL));
}

HParser *dns_question() {
    HParser *qname = dns_name();
    HParser *qtype = h_uint16();
    HParser *qclass = h_uint16();
    return h_sequence(qname, qtype, qclass, NULL);
}

HParser *dns_header() {
    HParser *id = h_uint16();
    HParser *flags = h_uint16();
    HParser *qdcount = h_uint16();
    HParser *ancount = h_uint16();
    HParser *nscount = h_uint16();
    HParser *arcount = h_uint16();
    return h_sequence(id, flags, qdcount, ancount, nscount, arcount, NULL);
}

HParser *dns_message() {
    HParser *header = dns_header();
    HParser *questions = h_many(dns_question());
    return h_sequence(header, questions, NULL);
}

int main(int argc, char **argv) {
    HParser *parser = dns_message();
    // Additional code to use the parser would go here
    h_parser_free(parser);
    return 0;
}