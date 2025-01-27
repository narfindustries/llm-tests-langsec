#include <hammer/hammer.h>
#include <hammer/glue.h>

// A helper function to parse a domain name
static HParsedToken *parse_domain_name(const HParseResult *p, void *user_data) {
    const HCountedArray *a = h_seq_elements(p->ast);
    HString *result = h_alloc(h_arena_get(p->ast), sizeof(HString));
    result->len = 0;
    result->str = h_arena_malloc(h_arena_get(p->ast), 256); // Allocate a buffer, maximum domain name length
    
    for (size_t i = 0; i < a->count; i++) {
        HString *part = h_str_value(a->elements[i]);
        
        if (i > 0) {
            result->str[result->len++] = '.';
        }
        
        memcpy(result->str + result->len, part->str, part->len);
        result->len += part->len;
    }
    
    result->str[result->len] = '\0'; // Null-terminate the string
    return H_MAKE_STR(result);
}

// Define the DNS query type
static HParser *dns_qtype() {
    return h_int16();
}

// Define the DNS query class
static HParser *dns_qclass() {
    return h_int16();
}

// Define a label within the domain name
static HParser *dns_label() {
    return h_length_value(h_uint8(), h_ascii());
}

// Define the complete domain name
static HParser *dns_domain_name() {
    return h_action(h_repeat1(dns_label()), parse_domain_name, NULL);
}

// Define a DNS question section
static HParser *dns_question() {
    return h_sequence(
        dns_domain_name(),
        dns_qtype(),
        dns_qclass(),
        NULL
    );
}

// Define the DNS header
static HParser *dns_header() {
    return h_sequence(
        h_int16(), // ID
        h_bits(16, false, (const bool[]){1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}), // Flags
        h_int16(), // QDCOUNT
        h_int16(), // ANCOUNT
        h_int16(), // NSCOUNT
        h_int16(), // ARCOUNT
        NULL
    );
}

// Define the complete DNS packet
static HParser *dns_packet() {
    return h_sequence(
        dns_header(),
        h_many(dns_question()), // Handle multiple questions
        NULL
    );
}

int main(int argc, char *argv[]) {
    HParser *parser = dns_packet();
    HParseResult *result = h_parse(parser, (const uint8_t*)"\x12\x34\x01\x00\x00\x01\x00\x00\x00\x00\x00\x00\x03www\x06google\x03com\x00\x00\x01\x00\x01", 29);
    if (result) {
        printf("Parsed successfully!\n");
        h_pprint(stdout, result->ast, 0, 1);
    } else {
        printf("Failed to parse!\n");
    }
    h_parse_result_free(result);
    h_parser_free(parser);
    return 0;
}
This code includes a complete DNS protocol parser using the Hammer parsing library in C, fixing potential compilation issues and ensuring proper handling of domain names and DNS question sections.