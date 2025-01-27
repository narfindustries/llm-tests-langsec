#include <stdio.h>
#include <hammer/hammer.h>

// Forward declarations
static HParsedToken *act_identity(const HParseResult *p, void *user_data);

// Definitions for the DNS message format
static HParser *dns_header, *dns_question, *dns_resource_record, *dns_message;

// Parsing DNS Header section
static void init_dns_header() {
    dns_header = h_sequence(
        h_uint16(), // ID
        h_bits(1, false), // QR: Query (0) or Response (1)
        h_bits(4, false), // OPCODE
        h_bits(1, false), // AA: Authoritative Answer
        h_bits(1, false), // TC: Truncation
        h_bits(1, false), // RD: Recursion Desired
        h_bits(1, false), // RA: Recursion Available
        h_bits(3, false), // Z: Reserved for future
        h_bits(4, false), // RCODE: Response Code
        h_uint16(), // QDCOUNT: Question Count
        h_uint16(), // ANCOUNT: Answer Record Count
        h_uint16(), // NSCOUNT: Authority Record Count
        h_uint16(), // ARCOUNT: Additional Record Count
        NULL
    );
}

// Parsing DNS Question section
static void init_dns_question() {
    dns_question = h_sequence(
        h_many1(h_uint8()),
        h_bytes(2), // Type
        h_bytes(2), // Class
        NULL
    );
}

// Parsing DNS Resource Record section which uses some recursive conventions
static void init_dns_resource_record() {
    dns_resource_record = h_sequence(
        h_many1(h_uint8()),
        h_bytes(2), // Type
        h_bytes(2), // Class
        h_uint32(), // TTL
        h_length_value(h_uint16(), h_any()), // RDLENGTH and RDATA
        NULL
    );
}

// Put everything together in DNS Message
static void init_dns_message() {
    init_dns_header();
    init_dns_question();
    init_dns_resource_record();
    dns_message = h_sequence(
        dns_header,
        h_many(dns_question, act_identity, NULL),
        h_many(dns_resource_record, act_identity, NULL),
        NULL
    );
}

// Example action to hook into parsers if specific processing is needed
static HParsedToken *act_identity(const HParseResult *p, void *user_data) {
    return h_make_seq_empty();
}

int main(int argc, char **argv) {
    // Initialize parsers
    init_dns_message();

    // Example usage
    HParser *final_parser = dns_message;

    // Parsing some binary data
    char data[] = {0x12, 0x34, 0xab, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x07, 'e', 'x', 'a', 'm', 'p', 'l', 'e', 0x03, 'c', 'o', 'm', 0x00, 0x00, 0x01, 0x00, 0x01};
    size_t length = sizeof(data) / sizeof(data[0]);

    HParseResult *result = h_parse(final_parser, data, length);
    if (result) {
        printf("Parsing Successful!\n");
    } else {
        printf("Parsing Failed!\n");
    }

    // Cleanup
    h_parse_result_free(result);
    h_parser_free(final_parser);

    return 0;
}