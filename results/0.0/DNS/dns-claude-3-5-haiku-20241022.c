#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <arpa/inet.h>
#include <hammer/hammer.h>
#include <hammer/parsers.h>

typedef struct {
    uint16_t transaction_id;
    struct {
        uint8_t qr:1;
        uint8_t opcode:4;
        uint8_t aa:1;
        uint8_t tc:1;
        uint8_t rd:1;
        uint8_t ra:1;
        uint8_t z:3;
        uint8_t rcode:4;
    } __attribute__((packed)) flags;
    uint16_t qdcount;
    uint16_t ancount;
    uint16_t nscount;
    uint16_t arcount;
} DNSHeader;

typedef struct {
    char* name;
    uint16_t type;
    uint16_t class;
} DNSQuestion;

typedef struct {
    char* name;
    uint16_t type;
    uint16_t class;
    uint32_t ttl;
    uint16_t rdlength;
    uint8_t* rdata;
} DNSResourceRecord;

typedef struct {
    DNSHeader header;
    DNSQuestion* questions;
    DNSResourceRecord* answers;
    DNSResourceRecord* authorities;
    DNSResourceRecord* additionals;
    uint16_t question_count;
    uint16_t answer_count;
    uint16_t authority_count;
    uint16_t additional_count;
} DNSMessage;

HParser* parse_dns_header(void) {
    return h_sequence(
        h_uint16(),   // transaction_id
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

HParser* parse_dns_name(void) {
    return h_many1(
        h_sequence(
            h_length_value(h_uint8(), h_ch_range(0x20, 0x7E)),
            h_end_p()
        )
    );
}

HParser* parse_dns_question(void) {
    return h_sequence(
        parse_dns_name(),   // name
        h_uint16(),         // type
        h_uint16()          // class
    );
}

HParser* parse_dns_resource_record(void) {
    return h_sequence(
        parse_dns_name(),   // name
        h_uint16(),         // type
        h_uint16(),         // class
        h_uint32(),         // ttl
        h_length_value(h_uint16(), h_uint8())  // rdata
    );
}

HParser* parse_dns_message(void) {
    return h_sequence(
        parse_dns_header(),
        h_repeat_n(parse_dns_question(), 1),
        h_repeat_n(parse_dns_resource_record(), 1),
        h_repeat_n(parse_dns_resource_record(), 1),
        h_repeat_n(parse_dns_resource_record(), 1)
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

    HParser* dns_parser = parse_dns_message();
    HParseResult* result = h_parse(dns_parser, buffer, file_size);

    if (result && result->ast) {
        printf("DNS message parsed successfully\n");
    } else {
        printf("DNS message parsing failed\n");
    }

    h_parse_result_free(result);
    h_arena_free(h_parser_arena(dns_parser));
    free(buffer);

    return 0;
}