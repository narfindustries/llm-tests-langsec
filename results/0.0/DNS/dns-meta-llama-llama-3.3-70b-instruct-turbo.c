#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define HAMMER_HEADER_ONLY
#define HAMMER_NO_MAIN

typedef struct {
    uint16_t id;
    uint8_t qr:1;
    uint8_t opcode:4;
    uint8_t aa:1;
    uint8_t tc:1;
    uint8_t rd:1;
    uint8_t ra:1;
    uint8_t z:3;
    uint8_t rcode:4;
    uint16_t qdcount;
    uint16_t ancount;
    uint16_t nscount;
    uint16_t arcount;
} dns_header_t;

typedef struct {
    uint8_t* name;
    uint16_t type;
    uint16_t class;
} dns_question_t;

typedef struct {
    uint8_t* name;
    uint16_t type;
    uint16_t class;
    uint32_t ttl;
    uint16_t rdlength;
    uint8_t* rdata;
} dns_rr_t;

typedef struct {
    dns_header_t header;
    dns_question_t* questions;
    dns_rr_t* answers;
    dns_rr_t* authorities;
    dns_rr_t* additionals;
} dns_message_t;

void hammer_define_dns_header(void) {
    HAMMER_BITS(dns_header_t, id, 16);
    HAMMER_BITS(dns_header_t, qr, 1);
    HAMMER_BITS(dns_header_t, opcode, 4);
    HAMMER_BITS(dns_header_t, aa, 1);
    HAMMER_BITS(dns_header_t, tc, 1);
    HAMMER_BITS(dns_header_t, rd, 1);
    HAMMER_BITS(dns_header_t, ra, 1);
    HAMMER_BITS(dns_header_t, z, 3);
    HAMMER_BITS(dns_header_t, rcode, 4);
    HAMMER_BITS(dns_header_t, qdcount, 16);
    HAMMER_BITS(dns_header_t, ancount, 16);
    HAMMER_BITS(dns_header_t, nscount, 16);
    HAMMER_BITS(dns_header_t, arcount, 16);
}

void hammer_define_dns_question(void) {
    HAMMER_BYTES(dns_question_t, name, HAMMER_PRED(is_domain_name));
    HAMMER_BITS(dns_question_t, type, 16);
    HAMMER_BITS(dns_question_t, class, 16);
}

void hammer_define_dns_rr(void) {
    HAMMER_BYTES(dns_rr_t, name, HAMMER_PRED(is_domain_name));
    HAMMER_BITS(dns_rr_t, type, 16);
    HAMMER_BITS(dns_rr_t, class, 16);
    HAMMER_BITS(dns_rr_t, ttl, 32);
    HAMMER_BITS(dns_rr_t, rdlength, 16);
    HAMMER_BYTES(dns_rr_t, rdata, rdlength);
}

void hammer_define_dns_message(void) {
    HAMMER_STRUCT(dns_message_t, header, hammer_define_dns_header);
    HAMMER_ARRAY(dns_message_t, questions, dns_question_t, header.qdcount, hammer_define_dns_question);
    HAMMER_ARRAY(dns_message_t, answers, dns_rr_t, header.ancount, hammer_define_dns_rr);
    HAMMER_ARRAY(dns_message_t, authorities, dns_rr_t, header.nscount, hammer_define_dns_rr);
    HAMMER_ARRAY(dns_message_t, additionals, dns_rr_t, header.arcount, hammer_define_dns_rr);
}

int is_domain_name(const uint8_t* data, size_t len) {
    size_t i = 0;
    while (i < len) {
        size_t label_len = data[i];
        if (label_len == 0) {
            return 1;
        }
        if (label_len > 63) {
            return 0;
        }
        i += label_len + 1;
    }
    return 0;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file: %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t file_size = ftell(file);
    rewind(file);

    uint8_t* data = malloc(file_size);
    if (!data) {
        printf("Error allocating memory\n");
        return 1;
    }

    size_t read_size = fread(data, 1, file_size, file);
    if (read_size != file_size) {
        printf("Error reading file\n");
        return 1;
    }

    fclose(file);

    dns_message_t message;
    if (!HAMMER_PARSE(dns_message_t, data, file_size, &message)) {
        printf("Error parsing DNS message\n");
        return 1;
    }

    printf("ID: %u\n", message.header.id);
    printf("QR: %u\n", message.header.qr);
    printf("OPCODE: %u\n", message.header.opcode);
    printf("AA: %u\n", message.header.aa);
    printf("TC: %u\n", message.header.tc);
    printf("RD: %u\n", message.header.rd);
    printf("RA: %u\n", message.header.ra);
    printf("Z: %u\n", message.header.z);
    printf("RCODE: %u\n", message.header.rcode);
    printf("QDCOUNT: %u\n", message.header.qdcount);
    printf("ANCOUNT: %u\n", message.header.ancount);
    printf("NSCOUNT: %u\n", message.header.nscount);
    printf("ARCOUNT: %u\n", message.header.arcount);

    for (size_t i = 0; i < message.header.qdcount; i++) {
        printf("Question %zu:\n", i);
        printf("  Name: %s\n", message.questions[i].name);
        printf("  Type: %u\n", message.questions[i].type);
        printf("  Class: %u\n", message.questions[i].class);
    }

    for (size_t i = 0; i < message.header.ancount; i++) {
        printf("Answer %zu:\n", i);
        printf("  Name: %s\n", message.answers[i].name);
        printf("  Type: %u\n", message.answers[i].type);
        printf("  Class: %u\n", message.answers[i].class);
        printf("  TTL: %u\n", message.answers[i].ttl);
        printf("  RDLENGTH: %u\n", message.answers[i].rdlength);
        printf("  RDATA: %s\n", message.answers[i].rdata);
    }

    for (size_t i = 0; i < message.header.nscount; i++) {
        printf("Authority %zu:\n", i);
        printf("  Name: %s\n", message.authorities[i].name);
        printf("  Type: %u\n", message.authorities[i].type);
        printf("  Class: %u\n", message.authorities[i].class);
        printf("  TTL: %u\n", message.authorities[i].ttl);
        printf("  RDLENGTH: %u\n", message.authorities[i].rdlength);
        printf("  RDATA: %s\n", message.authorities[i].rdata);
    }

    for (size_t i = 0; i < message.header.arcount; i++) {
        printf("Additional %zu:\n", i);
        printf("  Name: %s\n", message.additionals[i].name);
        printf("  Type: %u\n", message.additionals[i].type);
        printf("  Class: %u\n", message.additionals[i].class);
        printf("  TTL: %u\n", message.additionals[i].ttl);
        printf("  RDLENGTH: %u\n", message.additionals[i].rdlength);
        printf("  RDATA: %s\n", message.additionals[i].rdata);
    }

    free(data);
    return 0;
}