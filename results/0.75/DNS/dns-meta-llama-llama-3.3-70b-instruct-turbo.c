#include <hammer/hammer.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUFFER_SIZE 1024

typedef enum {
    QUERY,
    RESPONSE
} message_type;

typedef enum {
    A,
    NS,
    MD,
    MF,
    CNAME,
    SOA,
    MB,
    MG,
    MR,
    NULL_RR,
    WKS,
    PTR,
    HINFO,
    MINFO,
    MX,
    TXT
} rr_type;

typedef enum {
    IN,
    CS,
    CH,
    HS
} rr_class;

typedef struct {
    uint16_t id;
    message_type type;
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
} header;

typedef struct {
    char* name;
    rr_type type;
    rr_class class_;
} question;

typedef struct {
    char* name;
    rr_type type;
    rr_class class_;
    uint32_t ttl;
    uint16_t rdlength;
    char* rdata;
} rr;

typedef struct {
    header header;
    question* questions;
    rr* answers;
    rr* authorities;
    rr* additionals;
} dns_message;

void print_header(header* header) {
    printf("ID: %u\n", header->id);
    printf("Type: %s\n", header->type == QUERY ? "Query" : "Response");
    printf("Opcode: %u\n", header->opcode);
    printf("AA: %u\n", header->aa);
    printf("TC: %u\n", header->tc);
    printf("RD: %u\n", header->rd);
    printf("RA: %u\n", header->ra);
    printf("Z: %u\n", header->z);
    printf("Rcode: %u\n", header->rcode);
    printf("QDCount: %u\n", header->qdcount);
    printf("ANCount: %u\n", header->ancount);
    printf("NSCount: %u\n", header->nscount);
    printf("ARCount: %u\n", header->arcount);
}

void print_question(question* question) {
    printf("Name: %s\n", question->name);
    printf("Type: %u\n", question->type);
    printf("Class: %u\n", question->class_);
}

void print_rr(rr* rr) {
    printf("Name: %s\n", rr->name);
    printf("Type: %u\n", rr->type);
    printf("Class: %u\n", rr->class_);
    printf("TTL: %u\n", rr->ttl);
    printf("RDLength: %u\n", rr->rdlength);
    printf("RData: %s\n", rr->rdata);
}

void print_dns_message(dns_message* message) {
    print_header(&message->header);
    for (int i = 0; i < message->header.qdcount; i++) {
        print_question(&message->questions[i]);
    }
    for (int i = 0; i < message->header.ancount; i++) {
        print_rr(&message->answers[i]);
    }
    for (int i = 0; i < message->header.nscount; i++) {
        print_rr(&message->authorities[i]);
    }
    for (int i = 0; i < message->header.arcount; i++) {
        print_rr(&message->additionals[i]);
    }
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    char buffer[BUFFER_SIZE];
    size_t bytes_read = fread(buffer, 1, BUFFER_SIZE, file);
    if (bytes_read == 0) {
        printf("Error reading file\n");
        fclose(file);
        return 1;
    }

    HParser* parser = h_tuple(
        h_uint16(),
        h_uint8(),
        h_uint8(),
        h_uint8(),
        h_uint8(),
        h_uint8(),
        h_uint8(),
        h_uint8(),
        h_uint8(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint16()
    );

    HParseResult* result = h_parse(parser, (const uint8_t*)buffer, bytes_read);
    if (result != NULL) {
        dns_message* message = (dns_message*)result;
        print_dns_message(message);
    } else {
        printf("Error parsing DNS message\n");
    }

    fclose(file);
    return 0;
}