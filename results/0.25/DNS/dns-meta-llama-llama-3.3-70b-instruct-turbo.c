#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    uint16_t transcation_id;
    uint16_t flags;
    uint16_t qdcount;
    uint16_t ancount;
    uint16_t nscount;
    uint16_t arcount;
} dns_header_t;

typedef struct {
    uint16_t type;
    uint16_t class_;
} dns_question_t;

typedef struct {
    uint16_t type;
    uint16_t class_;
    uint32_t ttl;
    uint16_t rdlength;
    char rdata[256];
} dns_answer_t;

int main() {
    char dns_message[1024];
    dns_header_t* header = (dns_header_t*) dns_message;
    dns_question_t* question = (dns_question_t*) (dns_message + sizeof(dns_header_t));
    dns_answer_t* answer = (dns_answer_t*) (dns_message + sizeof(dns_header_t) + sizeof(dns_question_t));

    // Sample values for demonstration purposes
    header->transcation_id = 0x1234;
    header->flags = 0x8400;
    header->qdcount = 1;
    header->ancount = 1;
    header->nscount = 0;
    header->arcount = 0;

    question->type = 1;
    question->class_ = 1;

    answer->type = 1;
    answer->class_ = 1;
    answer->ttl = 3600;
    answer->rdlength = 4;
    sprintf(answer->rdata, "%d.%d.%d.%d", 192, 168, 1, 1);

    return 0;
}