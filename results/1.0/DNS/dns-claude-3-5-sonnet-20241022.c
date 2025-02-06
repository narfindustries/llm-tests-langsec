#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// Forward declarations
static HParser* dns_header();
static HParser* dns_question();
static HParser* dns_resource_record();
static HParser* dns_message();
static HParser* domain_name();

// DNS Header fields
static HParser* dns_id() {
    return h_uint16();
}

static HParser* dns_flags() {
    return h_bits(16, false);
}

static HParser* dns_counts() {
    return h_repeat_n(h_uint16(), 4);
}

// Domain name parsing
static HParser* label() {
    return h_sequence(h_uint8(), h_length_value(h_uint8(), h_uint8()), NULL);
}

static HParser* domain_name() {
    return h_many(label());
}

// Question section
static HParser* dns_qtype() {
    return h_uint16();
}

static HParser* dns_qclass() {
    return h_uint16();
}

static HParser* dns_question() {
    return h_sequence(domain_name(), dns_qtype(), dns_qclass(), NULL);
}

// Resource Record fields
static HParser* dns_ttl() {
    return h_uint32();
}

static HParser* dns_rdlength() {
    return h_uint16();
}

static HParser* dns_rdata() {
    return h_length_value(dns_rdlength(), h_uint8());
}

static HParser* dns_resource_record() {
    return h_sequence(domain_name(), dns_qtype(), dns_qclass(), dns_ttl(), dns_rdata(), NULL);
}

// Main DNS message parser
static HParser* dns_message() {
    return h_sequence(
        dns_header(),
        h_many(dns_question()),
        h_many(dns_resource_record()),     // Answer section
        h_many(dns_resource_record()),     // Authority section
        h_many(dns_resource_record()),     // Additional section
        NULL
    );
}

static HParser* dns_header() {
    return h_sequence(
        dns_id(),
        dns_flags(),
        dns_counts(),
        NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <dns_binary_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    // Get file size
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    // Read file contents
    uint8_t* input = malloc(file_size);
    if (!input) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    size_t bytes_read = fread(input, 1, file_size, file);
    fclose(file);

    if (bytes_read != file_size) {
        fprintf(stderr, "Failed to read entire file\n");
        free(input);
        return 1;
    }

    // Initialize parser
    HParser* dns_parser = dns_message();
    
    // Parse input
    HParseResult* result = h_parse(dns_parser, input, file_size);
    
    if (!result) {
        fprintf(stderr, "Failed to parse DNS message\n");
        free(input);
        return 1;
    }

    // TODO: Process parse result as needed
    h_parse_result_free(result);
    free(input);
    return 0;
}