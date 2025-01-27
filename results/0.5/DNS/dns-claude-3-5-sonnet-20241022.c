#include <hammer/hammer.h>
#include <stdio.h>

static const HParser* init_dns_parser() {
    // Basic components
    H_RULE(label_char, h_not_in("\0\n\r."));
    H_RULE(label, h_sequence(h_length_value(h_uint8(), label_char), NULL));
    H_RULE(domain_name, h_sepBy1(label, h_ch('.')));
    
    // Record types
    H_RULE(record_type, h_choice(h_token("A", 1),
                                h_token("NS", 2),
                                h_token("CNAME", 5),
                                h_token("MX", 2),
                                h_token("TXT", 3),
                                NULL));
    
    // TTL (Time To Live)
    H_RULE(ttl, h_uint32());
    
    // IP address components
    H_RULE(ip_octet, h_int_range(h_uint8(), 0, 255));
    H_RULE(ip_address, h_sepBy1(ip_octet, h_ch('.')));
    
    // Record data based on type
    H_RULE(record_data, h_choice(ip_address,      // For A records
                                domain_name,       // For NS, CNAME records
                                h_sequence(h_uint16(), domain_name, NULL),  // For MX records
                                h_token_skip(h_whitespace(), h_many1(label_char)), // For TXT records
                                NULL));
    
    // Complete DNS record
    H_RULE(dns_record, h_sequence(domain_name,
                                 h_whitespace(),
                                 ttl,
                                 h_whitespace(),
                                 record_type,
                                 h_whitespace(),
                                 record_data,
                                 h_optional(h_whitespace()),
                                 NULL));
    
    // Complete DNS zone file (multiple records)
    H_RULE(dns_zone, h_many1(dns_record));
    
    return dns_zone;
}

int main() {
    const HParser* parser = init_dns_parser();
    
    // Example input
    const uint8_t input[] = "example.com. 3600 A 192.168.1.1\n"
                           "example.com. 3600 MX 10 mail.example.com.\n"
                           "mail.example.com. 3600 A 192.168.1.2";
    
    HParseResult* result = h_parse(parser, input, strlen((char*)input));
    
    if(result) {
        printf("Parsing successful\n");
        h_parse_result_free(result);
        return 0;
    } else {
        printf("Parsing failed\n");
        return 1;
    }
}