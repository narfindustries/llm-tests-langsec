#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// Define parsers for DNS fields
HParser *dns_name_parser() {
    return h_sequence(h_uint8(), h_length_value(h_uint8(), h_sequence(h_uint8(), NULL)), NULL);
}

HParser *dns_header_parser() {
    return h_sequence(
        h_uint16(), // ID
        h_uint8(),  // QR, OPCODE, AA, TC, RD
        h_uint8(),  // RA, Z, RCODE
        h_uint16(), // QDCOUNT
        h_uint16(), // ANCOUNT
        h_uint16(), // NSCOUNT
        h_uint16(), // ARCOUNT
        NULL
    );
}

HParser *dns_question_parser() {
    return h_sequence(
        dns_name_parser(), // QNAME
        h_uint16(),        // QTYPE
        h_uint16(),        // QCLASS
        NULL
    );
}

HParser *dns_resource_record_parser() {
    return h_sequence(
        dns_name_parser(), // NAME
        h_uint16(),        // TYPE
        h_uint16(),        // CLASS
        h_uint32(),        // TTL
        h_uint16(),        // RDLENGTH
        h_length_value(h_uint16(), h_uint8()), // RDATA
        NULL
    );
}

HParser *dns_message_parser() {
    return h_sequence(
        dns_header_parser(), // Header
        h_repeat_n(dns_question_parser(), h_uint16()), // Question section
        h_repeat_n(dns_resource_record_parser(), h_uint16()), // Answer section
        h_repeat_n(dns_resource_record_parser(), h_uint16()), // Authority section
        h_repeat_n(dns_resource_record_parser(), h_uint16()), // Additional section
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(dns_message_parser(), buffer, file_size);
    if (result) {
        printf("Successfully parsed DNS message.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse DNS message.\n");
    }

    free(buffer);
    return 0;
}