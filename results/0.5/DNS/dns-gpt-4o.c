#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

typedef struct {
    uint16_t id;
    uint16_t flags;
    uint16_t qdcount;
    uint16_t ancount;
    uint16_t nscount;
    uint16_t arcount;
} dns_header_t;

typedef struct {
    char *qname;
    uint16_t qtype;
    uint16_t qclass;
} dns_question_t;

typedef struct {
    char *name;
    uint16_t type;
    uint16_t class;
    uint32_t ttl;
    uint16_t rdlength;
    uint8_t *rdata;
} dns_rr_t;

HParser *dns_header_parser;
HParser *dns_question_parser;
HParser *dns_rr_parser;
HParser *dns_message_parser;

HParser *create_dns_header_parser() {
    return h_sequence(
        h_uint16(), // ID
        h_uint16(), // Flags
        h_uint16(), // QDCOUNT
        h_uint16(), // ANCOUNT
        h_uint16(), // NSCOUNT
        h_uint16(), // ARCOUNT
        NULL
    );
}

HParser *create_dns_question_parser() {
    HParser *label = h_length_value(h_uint8(), h_repeat_n(h_uint8(), 0));
    HParser *qname = h_many1(label);
    return h_sequence(
        qname,      // QNAME
        h_uint16(), // QTYPE
        h_uint16(), // QCLASS
        NULL
    );
}

HParser *create_dns_rr_parser() {
    HParser *label = h_length_value(h_uint8(), h_repeat_n(h_uint8(), 0));
    HParser *name = h_many1(label);
    return h_sequence(
        name,       // NAME
        h_uint16(), // TYPE
        h_uint16(), // CLASS
        h_uint32(), // TTL
        h_length_value(h_uint16(), h_repeat_n(h_uint8(), 0)), // RDLENGTH + RDATA
        NULL
    );
}

HParser *create_dns_message_parser() {
    HParser *questions = h_many(create_dns_question_parser());
    HParser *answers = h_many(create_dns_rr_parser());
    HParser *authorities = h_many(create_dns_rr_parser());
    HParser *additionals = h_many(create_dns_rr_parser());

    return h_sequence(
        create_dns_header_parser(),
        questions, // QDCOUNT
        answers,   // ANCOUNT
        authorities, // NSCOUNT
        additionals, // ARCOUNT
        NULL
    );
}

void parse_dns_message(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(length);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    fread(buffer, 1, length, file);
    fclose(file);

    HParseResult *result = h_parse(dns_message_parser, buffer, length);
    if (result) {
        printf("DNS message parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse DNS message.\n");
    }

    free(buffer);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <dns_message_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    dns_message_parser = create_dns_message_parser();
    parse_dns_message(argv[1]);
    h_parser_free(dns_message_parser);

    return EXIT_SUCCESS;
}