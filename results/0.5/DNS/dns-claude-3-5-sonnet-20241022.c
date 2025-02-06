#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// DNS Header parsers
HParser* dns_id() {
    return h_uint16();
}

HParser* dns_flags() {
    return h_bits(16, false);
}

HParser* dns_counts() {
    return h_sequence(h_uint16(), h_uint16(), h_uint16(), h_uint16(), NULL);
}

// DNS Name parser
HParser* dns_label() {
    return h_sequence(h_uint8(), h_length_value(h_left, h_uint8()), NULL);
}

HParser* dns_name() {
    return h_many(dns_label());
}

// Question section parsers
HParser* dns_question() {
    return h_sequence(
        dns_name(),
        h_uint16(), // QTYPE
        h_uint16(), // QCLASS
        NULL
    );
}

// Resource Record parsers
HParser* dns_rr() {
    return h_sequence(
        dns_name(),           // NAME
        h_uint16(),          // TYPE
        h_uint16(),          // CLASS
        h_uint32(),          // TTL
        h_length_value(      // RDATA
            h_uint16(),      // RDLENGTH
            h_uint8()        // actual data
        ),
        NULL
    );
}

// Complete DNS message parser
HParser* dns_message() {
    return h_sequence(
        dns_id(),            // ID
        dns_flags(),         // Flags
        dns_counts(),        // QDCOUNT, ANCOUNT, NSCOUNT, ARCOUNT
        h_many(dns_question()), // Question section
        h_many(dns_rr()),    // Answer section
        h_many(dns_rr()),    // Authority section
        h_many(dns_rr()),    // Additional section
        NULL
    );
}

void print_parse_result(const HParsedToken* result) {
    if (!result) {
        printf("Parsing failed\n");
        return;
    }
    
    // Add printing logic here based on the parsed structure
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

    // Parse the input
    const HParsedToken* result = h_parse(dns_parser, buffer, file_size);
    print_parse_result(result);

    // Cleanup
    free(buffer);
    return 0;
}