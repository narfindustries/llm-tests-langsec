#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stddef.h>
#include <hammer/hammer.h>

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
    HParsedToken* qname;
    uint16_t qtype;
    uint16_t qclass;
} DNSQuestion;

typedef struct {
    HParsedToken* name;
    uint16_t type;
    uint16_t class;
    uint32_t ttl;
    HParsedToken* rdata;
} DNSResourceRecord;

typedef struct {
    DNSHeader header;
    HParsedToken* questions;
    HParsedToken* answers;
    HParsedToken* authorities;
    HParsedToken* additionals;
} DNSMessage;

HParser* dns_header_parser() {
    return h_sequence(
        h_uint16(),   // id
        h_bits(1, false),  // qr
        h_bits(4, false),  // opcode
        h_bits(1, false),  // aa
        h_bits(1, false),  // tc
        h_bits(1, false),  // rd
        h_bits(1, false),  // ra
        h_bits(3, false),  // z
        h_bits(4, false),  // rcode
        h_uint16(),   // qdcount
        h_uint16(),   // ancount
        h_uint16(),   // nscount
        h_uint16()    // arcount
    );
}

HParser* dns_name_parser() {
    return h_sequence(
        h_length_value(h_uint8(), h_many(h_int_range(h_ch('a'), h_ch('z')))),
        h_not(h_any_char())
    );
}

HParser* dns_question_parser() {
    return h_sequence(
        dns_name_parser(),  // qname
        h_uint16(),         // qtype
        h_uint16()          // qclass
    );
}

HParser* dns_resource_record_parser() {
    return h_sequence(
        dns_name_parser(),  // name
        h_uint16(),         // type
        h_uint16(),         // class
        h_uint32(),         // ttl
        h_length_value(h_uint16(), h_many(h_int_range(h_ch(0), h_ch(255))))  // rdata
    );
}

HParser* dns_message_parser() {
    return h_sequence(
        dns_header_parser(),
        h_repeat_n(dns_question_parser(), 1),
        h_repeat_n(dns_resource_record_parser(), 1),
        h_repeat_n(dns_resource_record_parser(), 1),
        h_repeat_n(dns_resource_record_parser(), 1)
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <dns_binary_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("File read error");
        free(buffer);
        fclose(file);
        return 1;
    }
    fclose(file);

    HParser* parser = dns_message_parser();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("DNS message parsed successfully\n");
        h_parse_result_free(result);
    } else {
        printf("DNS message parsing failed\n");
    }

    free(buffer);
    h_parser_free(parser);
    return 0;
}