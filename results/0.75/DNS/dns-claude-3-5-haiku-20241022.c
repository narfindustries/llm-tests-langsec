#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
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
    HBytes* name;
    uint16_t type;
    uint16_t class;
} DNSQuestion;

typedef struct {
    HBytes* name;
    uint16_t type;
    uint16_t class;
    uint32_t ttl;
    uint16_t rdlength;
    HBytes* rdata;
} DNSResourceRecord;

typedef struct {
    DNSHeader header;
    DNSQuestion* questions;
    DNSResourceRecord* answers;
    DNSResourceRecord* authorities;
    DNSResourceRecord* additionals;
    uint16_t num_questions;
    uint16_t num_answers;
    uint16_t num_authorities;
    uint16_t num_additionals;
} DNSMessage;

HParser* dns_header_parser() {
    return h_sequence(
        h_uint16(),   // ID
        h_bits(1, false),  // QR
        h_bits(4, false),  // OPCODE
        h_bits(1, false),  // AA
        h_bits(1, false),  // TC
        h_bits(1, false),  // RD
        h_bits(1, false),  // RA
        h_bits(3, false),  // Z
        h_bits(4, false),  // RCODE
        h_uint16(),   // QDCOUNT
        h_uint16(),   // ANCOUNT
        h_uint16(),   // NSCOUNT
        h_uint16()    // ARCOUNT
    );
}

HParser* dns_name_parser() {
    return h_choice(
        h_sequence(
            h_uint8(),
            h_not(h_ch(0))
        ),
        h_end_p()
    );
}

HParser* dns_question_parser() {
    return h_sequence(
        dns_name_parser(),  // QNAME
        h_uint16(),         // QTYPE
        h_uint16()          // QCLASS
    );
}

HParser* dns_resource_record_parser() {
    return h_sequence(
        dns_name_parser(),  // NAME
        h_uint16(),         // TYPE
        h_uint16(),         // CLASS
        h_uint32(),         // TTL
        h_uint16(),         // RDLENGTH
        h_repeat_n(h_uint8(), h_get_uint16())     // RDATA
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
        fprintf(stderr, "Usage: %s <dns_file>\n", argv[0]);
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
        perror("Memory allocation failed");
        fclose(file);
        return 1;
    }

    size_t read_size = fread(buffer, 1, file_size, file);
    fclose(file);

    if (read_size != file_size) {
        perror("File read error");
        free(buffer);
        return 1;
    }

    HParser* parser = dns_message_parser();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result && result->ast) {
        printf("DNS message parsed successfully\n");
    } else {
        printf("DNS message parsing failed\n");
    }

    h_parse_result_free(result);
    h_parser_free(parser);
    free(buffer);

    return 0;
}