#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <hammer/parsers/parser_internal.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

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
    char* qname;
    uint16_t qtype;
    uint16_t qclass;
} DNSQuestion;

typedef struct {
    char* name;
    uint16_t type;
    uint16_t class;
    uint32_t ttl;
    uint16_t rdlength;
    uint8_t* rdata;
} DNSResourceRecord;

static HParser* dns_header_parser;
static HParser* dns_question_parser;
static HParser* dns_rr_parser;
static HParser* dns_message_parser;

static HParsedToken* act_make_header(const HParseResult* p, void* user_data) {
    DNSHeader* header = (DNSHeader*)malloc(sizeof(DNSHeader));
    const HCountedArray* seq = H_CAST_SEQ(p->ast);
    uint16_t flags = H_CAST_UINT(seq->elements[1]);
    
    header->id = H_CAST_UINT(seq->elements[0]);
    header->qr = (flags >> 15) & 0x1;
    header->opcode = (flags >> 11) & 0xF;
    header->aa = (flags >> 10) & 0x1;
    header->tc = (flags >> 9) & 0x1;
    header->rd = (flags >> 8) & 0x1;
    header->ra = (flags >> 7) & 0x1;
    header->z = (flags >> 4) & 0x7;
    header->rcode = flags & 0xF;
    header->qdcount = H_CAST_UINT(seq->elements[2]);
    header->ancount = H_CAST_UINT(seq->elements[3]);
    header->nscount = H_CAST_UINT(seq->elements[4]);
    header->arcount = H_CAST_UINT(seq->elements[5]);
    
    HArena* arena = h_new_arena();
    HParsedToken* ret = h_make_seqn(arena, 1);
    ret->seq->elements[0] = (HParsedToken*)header;
    return ret;
}

static HParser* init_dns_header_parser(void) {
    return h_action(h_sequence(h_uint16(), h_uint16(),
                              h_uint16(), h_uint16(),
                              h_uint16(), h_uint16(),
                              NULL),
                    act_make_header, NULL);
}

static HParser* init_dns_label_parser(void) {
    return h_sequence(h_uint8(),
                     h_repeat_n(h_uint8(), h_uint8()),
                     NULL);
}

static HParser* init_dns_name_parser(void) {
    return h_many1(init_dns_label_parser());
}

static HParser* init_dns_question_parser(void) {
    return h_sequence(init_dns_name_parser(),
                     h_uint16(),
                     h_uint16(),
                     NULL);
}

static HParser* init_dns_rdata_parser(void) {
    return h_repeat_n(h_uint8(), h_uint16());
}

static HParser* init_dns_rr_parser(void) {
    return h_sequence(init_dns_name_parser(),
                     h_uint16(),
                     h_uint16(),
                     h_uint32(),
                     init_dns_rdata_parser(),
                     NULL);
}

static HParser* init_dns_message_parser(void) {
    return h_sequence(init_dns_header_parser(),
                     h_many(init_dns_question_parser()),
                     h_many(init_dns_rr_parser()),
                     h_many(init_dns_rr_parser()),
                     h_many(init_dns_rr_parser()),
                     NULL);
}

void init_parsers(void) {
    dns_header_parser = init_dns_header_parser();
    dns_question_parser = init_dns_question_parser();
    dns_rr_parser = init_dns_rr_parser();
    dns_message_parser = init_dns_message_parser();
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <dns_binary_file>\n", argv[0]);
        return 1;
    }

    FILE* f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t* buffer = malloc(size);
    if (fread(buffer, 1, size, f) != size) {
        perror("Failed to read file");
        fclose(f);
        free(buffer);
        return 1;
    }
    fclose(f);

    init_parsers();

    HParseResult* result = h_parse(dns_message_parser, buffer, size);
    if (!result) {
        fprintf(stderr, "Failed to parse DNS message\n");
        free(buffer);
        return 1;
    }

    printf("Successfully parsed DNS message\n");

    h_parse_result_free(result);
    free(buffer);
    return 0;
}