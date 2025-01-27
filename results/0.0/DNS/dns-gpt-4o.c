#include <hammer/hammer.h>

HParser *create_dns_parser() {
    // Define basic parsers
    HParser *label_length = h_uint8();
    HParser *label = h_repeat_n(h_uint8(), label_length);
    HParser *domain_name = h_many1(h_sequence(label, h_ch('.')));
    
    // Define DNS header fields
    HParser *dns_id = h_uint16();
    HParser *dns_flags = h_uint16();
    HParser *dns_qdcount = h_uint16();
    HParser *dns_ancount = h_uint16();
    HParser *dns_nscount = h_uint16();
    HParser *dns_arcount = h_uint16();
    
    // Define DNS question section
    HParser *qname = domain_name;
    HParser *qtype = h_uint16();
    HParser *qclass = h_uint16();
    HParser *dns_question = h_sequence(qname, qtype, qclass);
    
    // Define DNS resource record section
    HParser *rr_name = domain_name;
    HParser *rr_type = h_uint16();
    HParser *rr_class = h_uint16();
    HParser *rr_ttl = h_uint32();
    HParser *rr_rdlength = h_uint16();
    HParser *rr_rdata = h_repeat_n(h_uint8(), rr_rdlength);
    HParser *dns_rr = h_sequence(rr_name, rr_type, rr_class, rr_ttl, rr_rdlength, rr_rdata);
    
    // Define the complete DNS message parser
    HParser *dns_message = h_sequence(
        dns_id,
        dns_flags,
        dns_qdcount,
        dns_ancount,
        dns_nscount,
        dns_arcount,
        h_many(dns_question),
        h_many(dns_rr),
        h_many(dns_rr),
        h_many(dns_rr)
    );
    
    return dns_message;
}