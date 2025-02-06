#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef struct {
    uint16_t id;
    uint8_t qr;
    uint8_t opcode;
    uint8_t aa;
    uint8_t tc;
    uint8_t rd;
    uint8_t ra;
    uint8_t z;
    uint8_t rcode;
    uint16_t qdcount;
    uint16_t ancount;
    uint16_t nscount;
    uint16_t arcount;
} DNSHeader;

typedef struct {
    uint8_t *name;
    uint16_t type;
    uint16_t class;
} DNSQuestion;

typedef struct {
    uint8_t *name;
    uint16_t type;
    uint16_t class;
    uint32_t ttl;
    uint16_t rdlength;
    uint8_t *rdata;
} DNSResourceRecord;

typedef struct {
    DNSHeader header;
    DNSQuestion *questions;
    DNSResourceRecord *answers;
    DNSResourceRecord *authorities;
    DNSResourceRecord *additionals;
} DNSMessage;

HParser *dns_name_parser() {
    return h_sequence(h_length_value(h_uint8(), h_choice(h_uint8(), h_end_p())), h_end_p());
}

HParser *dns_header_parser() {
    return h_sequence(
        h_bits(16, NULL),
        h_bits(1, NULL),
        h_bits(4, NULL),
        h_bits(1, NULL),
        h_bits(1, NULL),
        h_bits(1, NULL),
        h_bits(1, NULL),
        h_bits(3, NULL),
        h_bits(4, NULL),
        h_bits(16, NULL),
        h_bits(16, NULL),
        h_bits(16, NULL),
        h_bits(16, NULL),
        NULL
    );
}

HParser *dns_question_parser() {
    return h_sequence(
        dns_name_parser(),
        h_bits(16, NULL),
        h_bits(16, NULL),
        NULL
    );
}

HParser *dns_resource_record_parser() {
    return h_sequence(
        dns_name_parser(),
        h_bits(16, NULL),
        h_bits(16, NULL),
        h_bits(32, NULL),
        h_bits(16, NULL),
        h_length_value(h_uint16(), h_uint8()),
        NULL
    );
}

HParser *dns_message_parser() {
    return h_sequence(
        dns_header_parser(),
        h_repeat_n(dns_question_parser(), h_uint16()),
        h_repeat_n(dns_resource_record_parser(), h_uint16()),
        h_repeat_n(dns_resource_record_parser(), h_uint16()),
        h_repeat_n(dns_resource_record_parser(), h_uint16()),
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

    uint8_t *buffer = (uint8_t *)malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = dns_message_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);
    if (!result) {
        fprintf(stderr, "Failed to parse DNS message\n");
        free(buffer);
        return 1;
    }

    DNSMessage *dns_message = (DNSMessage *)result->ast;
    printf("DNS Message Parsed Successfully\n");

    h_parse_result_free(result);
    free(buffer);
    return 0;
}