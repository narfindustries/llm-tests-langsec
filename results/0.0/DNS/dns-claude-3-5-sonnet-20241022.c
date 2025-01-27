#include <hammer/hammer.h>
#include <stdio.h>

static const HParser* init_dns_parser(void) {
    // Basic components
    H_RULE(label_char, h_not_in("\0\n\r."));
    H_RULE(label, h_sequence(h_many1(label_char), NULL));
    H_RULE(dot, h_ch('.'));
    
    // Domain name as sequence of labels separated by dots
    H_RULE(domain_name, h_sequence(label, 
                                 h_many(h_sequence(dot, label, NULL)), 
                                 NULL));

    // Record types
    H_RULE(type_a, h_token("A", 1));
    H_RULE(type_aaaa, h_token("AAAA", 4));
    H_RULE(type_mx, h_token("MX", 2));
    H_RULE(type_txt, h_token("TXT", 3));
    H_RULE(record_type, h_choice(type_a, type_aaaa, type_mx, type_txt, NULL));

    // TTL as digits
    H_RULE(ttl, h_many1(h_digit()));

    // Priority for MX records
    H_RULE(priority, h_many1(h_digit()));

    // IPv4 octet
    H_RULE(ipv4_octet, h_int_range(h_uint8(), 0, 255));
    H_RULE(ipv4_addr, h_sequence(ipv4_octet,
                                h_ch('.'), ipv4_octet,
                                h_ch('.'), ipv4_octet,
                                h_ch('.'), ipv4_octet,
                                NULL));

    // IPv6 components
    H_RULE(hex_digit, h_choice(h_ch_range('0', '9'),
                              h_ch_range('a', 'f'),
                              h_ch_range('A', 'F'),
                              NULL));
    H_RULE(ipv6_group, h_repeat_n(hex_digit, 1, 4));
    H_RULE(ipv6_addr, h_sequence(ipv6_group,
                                h_repeat_n(h_sequence(h_ch(':'), ipv6_group, NULL), 7, 7),
                                NULL));

    // Text record value
    H_RULE(txt_char, h_not_in("\n\r"));
    H_RULE(txt_value, h_many1(txt_char));

    // Record value based on type
    H_RULE(record_value, h_choice(ipv4_addr,      // For A records
                                 ipv6_addr,        // For AAAA records
                                 h_sequence(priority, h_whitespace(), domain_name, NULL),  // For MX
                                 txt_value,        // For TXT
                                 NULL));

    // Complete DNS record
    H_RULE(dns_record, h_sequence(domain_name,
                                 h_whitespace(),
                                 ttl,
                                 h_whitespace(),
                                 record_type,
                                 h_whitespace(),
                                 record_value,
                                 h_optional(h_whitespace()),
                                 NULL));

    // Complete DNS zone file
    return h_many(h_sequence(dns_record, h_optional(h_whitespace()), NULL));
}

int main(int argc, char* argv[]) {
    const HParser* parser = init_dns_parser();
    
    if (!parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }

    return 0;
}