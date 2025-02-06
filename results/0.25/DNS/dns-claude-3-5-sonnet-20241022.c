#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// DNS Header parsers
HParser* dns_id() { return h_uint16(); }

HParser* dns_flags() {
    return h_bits(16, false);  // QR(1) + OPCODE(4) + AA(1) + TC(1) + RD(1) + RA(1) + Z(3) + RCODE(4)
}

HParser* dns_counts() {
    return h_sequence(h_uint16(), h_uint16(), h_uint16(), h_uint16(), NULL);
}

// Label parsing for domain names
HParser* dns_label() {
    return h_sequence(
        h_uint8(),                    // length
        h_length_value(h_uint8(), h_uint8()), // actual label
        NULL
    );
}

// Domain name parser (sequence of labels ending with zero length)
HParser* dns_name() {
    return h_many(dns_label());
}

// Question section parsers
HParser* dns_question() {
    return h_sequence(
        dns_name(),   // QNAME
        h_uint16(),   // QTYPE
        h_uint16(),   // QCLASS
        NULL
    );
}

// Resource Record parser
HParser* dns_rr() {
    return h_sequence(
        dns_name(),   // NAME
        h_uint16(),   // TYPE
        h_uint16(),   // CLASS
        h_uint32(),   // TTL
        h_length_value(h_uint16(), h_uint8()), // RDLENGTH + RDATA
        NULL
    );
}

// Complete DNS message parser
HParser* dns_message() {
    return h_sequence(
        dns_id(),     // ID
        dns_flags(),  // Flags
        dns_counts(), // QDCOUNT, ANCOUNT, NSCOUNT, ARCOUNT
        h_many(dns_question()), // Question section
        h_many(dns_rr()),      // Answer section
        h_many(dns_rr()),      // Authority section
        h_many(dns_rr()),      // Additional section
        NULL
    );
}

void print_parse_result(const HParsedToken* result) {
    if (!result) {
        printf("Parse failed\n");
        return;
    }

    // Add parsing result printing logic here
    printf("Successfully parsed DNS message\n");
}

int main(int argc, char** argv) {
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

    // Read file content
    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    size_t read_size = fread(buffer, 1, file_size, file);
    fclose(file);

    if (read_size != file_size) {
        fprintf(stderr, "Failed to read entire file\n");
        free(buffer);
        return 1;
    }

    // Initialize parser
    HParser* dns_parser = dns_message();
    if (!dns_parser) {
        fprintf(stderr, "Failed to create parser\n");
        free(buffer);
        return 1;
    }

    // Parse the DNS message
    const HParsedToken* result = h_parse(dns_parser, buffer, file_size);
    print_parse_result(result);

    // Cleanup
    free(buffer);
    return result ? 0 : 1;
}