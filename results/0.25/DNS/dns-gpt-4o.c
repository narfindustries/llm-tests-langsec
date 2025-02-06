#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
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

HParser *dns_name_parser() {
    return h_many1(h_choice(h_uint8(), h_end_p()));
}

HParser *dns_header_parser() {
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

HParser *dns_question_parser() {
    return h_sequence(
        dns_name_parser(), // QNAME
        h_uint16(),        // QTYPE
        h_uint16(),        // QCLASS
        NULL
    );
}

HParsedToken *dns_rr_bind_func(HAllocator *mm, const HParsedToken *token, void *user_data) {
    const HParsedToken *seq = token;
    uint16_t rdlength = h_ntohs(h_seq_index(seq, 4)->uint16);
    HParsedToken *rdata = h_token_bytes(mm, rdlength, h_seq_index(seq, 5)->bytes);
    return h_token_seq(mm, 6, h_seq_index(seq, 0), h_seq_index(seq, 1), h_seq_index(seq, 2), h_seq_index(seq, 3), h_seq_index(seq, 4), rdata);
}

HParser *dns_rr_parser() {
    return h_bind(
        h_sequence(
            dns_name_parser(), // NAME
            h_uint16(),        // TYPE
            h_uint16(),        // CLASS
            h_uint32(),        // TTL
            h_uint16(),        // RDLENGTH
            h_length_value(h_uint16(), h_uint8()), // RDATA
            NULL
        ),
        dns_rr_bind_func,
        NULL
    );
}

HParser *dns_message_parser() {
    return h_sequence(
        dns_header_parser(),
        h_many(dns_question_parser()),
        h_many(dns_rr_parser()),
        h_many(dns_rr_parser()),
        h_many(dns_rr_parser()),
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
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = dns_message_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("DNS message parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse DNS message.\n");
    }

    free(buffer);
    h_parser_free(parser);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <dns_binary_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    parse_dns_message(argv[1]);
    return EXIT_SUCCESS;
}