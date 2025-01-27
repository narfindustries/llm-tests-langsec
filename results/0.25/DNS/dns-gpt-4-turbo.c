#include <hammer/hammer.h>
#include <hammer/glue.h>

// Forward declarations for parsers
static HParsedToken *act_ip_address(const HParseResult *p, void *user_data);
static HParsedToken *act_domain_name(const HParseResult *p, void *user_data);
static HParsedToken *act_dns_query(const HParseResult *p, void *user_data);
static HParsedToken *act_dns_response(const HParseResult *p, void *user_data);

// Basic parsers
static HParser *ipv4_address;
static HParser *domain_label;
static HParser *domain_name;
static HParser *dns_type;
static HParser *dns_class;
static HParser *dns_query;
static HParser *dns_response;

// Action functions
static HParsedToken *act_ip_address(const HParseResult *p, void *user_data) {
    // Convert parsed byte sequence to IP address string
    const uint8_t *bytes = h_seq_elements(p->ast, 0);
    char *ip_str = malloc(16); // Enough for "xxx.xxx.xxx.xxx\0"
    sprintf(ip_str, "%u.%u.%u.%u", bytes[0], bytes[1], bytes[2], bytes[3]);
    return H_MAKE_STR(ip_str);
}

static HParsedToken *act_domain_name(const HParseResult *p, void *user_data) {
    // Concatenate labels to form a domain name
    HCountedArray *labels = h_seq_elements(p->ast, 0);
    HString *s = h_string_new("");
    for (size_t i = 0; i < labels->len; i++) {
        if (i > 0) h_string_concat_cstr(s, ".");
        h_string_concat(s, h_str_value(h_array_index(labels, i)));
    }
    return H_MAKE_STR(s->str);
}

static HParsedToken *act_dns_query(const HParseResult *p, void *user_data) {
    // Extract and return DNS query fields
    return h_action_ok(p, user_data);
}

static HParsedToken *act_dns_response(const HParseResult *p, void *user_data) {
    // Extract and return DNS response fields
    return h_action_ok(p, user_data);
}

// Parser definitions
void init_parsers() {
    ipv4_address = h_action(h_repeat_n(h_uint8(), 4), act_ip_address, NULL);

    domain_label = h_action(h_length_value(h_uint8(), h_ch_range('a', 'z')), h_args(), NULL);
    domain_name = h_action(h_sepBy1(domain_label, h_ch('.')), act_domain_name, NULL);

    dns_type = h_uint16();
    dns_class = h_uint16();

    dns_query = h_action(h_sequence(domain_name, dns_type, dns_class, NULL), act_dns_query, NULL);
    dns_response = h_action(h_sequence(domain_name, dns_type, dns_class, ipv4_address, NULL), act_dns_response, NULL);
}

int main(int argc, char **argv) {
    init_parsers();

    // Example usage
    const uint8_t input_data[] = {3, 'w', 'w', 'w', 6, 'g', 'o', 'o', 'g', 'l', 'e', 3, 'c', 'o', 'm', 0, 0, 1, 0, 1};
    HParseResult *result = h_parse(dns_query, input_data, sizeof(input_data));
    if (result) {
        printf("Parsed DNS Query: %s\n", H_CAST_STR(result->ast));
        h_parse_result_free(result);
    } else {
        printf("Failed to parse DNS Query\n");
    }

    return 0;
}