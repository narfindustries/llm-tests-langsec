#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define DNS_HEADER_SIZE 12

typedef enum {
    DNS_QR_QUERY = 0,
    DNS_QR_RESPONSE = 1
} dns_qr_t;

typedef enum {
    DNS_OPCODE_QUERY = 0,
    DNS_OPCODE_IQUERY = 1,
    DNS_OPCODE_STATUS = 2
} dns_opcode_t;

typedef enum {
    DNS_RCODE_NO_ERROR = 0,
    DNS_RCODE_FORMAT_ERROR = 1,
    DNS_RCODE_SERVER_FAILURE = 2,
    DNS_RCODE_NAME_ERROR = 3,
    DNS_RCODE_NOT_IMPLEMENTED = 4,
    DNS_RCODE_REFUSED = 5
} dns_rcode_t;

typedef enum {
    DNS_TYPE_A = 1,
    DNS_TYPE_NS = 2,
    DNS_TYPE_MD = 3,
    DNS_TYPE_MF = 4,
    DNS_TYPE_CNAME = 5,
    DNS_TYPE_SOA = 6,
    DNS_TYPE_MB = 7,
    DNS_TYPE_MG = 8,
    DNS_TYPE_MR = 9,
    DNS_TYPE_NULL = 10,
    DNS_TYPE_WKS = 11,
    DNS_TYPE_PTR = 12,
    DNS_TYPE_HINFO = 13,
    DNS_TYPE_MINFO = 14,
    DNS_TYPE_MX = 15,
    DNS_TYPE_TXT = 16
} dns_type_t;

typedef enum {
    DNS_CLASS_IN = 1,
    DNS_CLASS_CS = 2,
    DNS_CLASS_CH = 3,
    DNS_CLASS_HS = 4
} dns_class_t;

typedef struct {
    uint16_t id;
    dns_qr_t qr;
    dns_opcode_t opcode;
    uint8_t aa;
    uint8_t tc;
    uint8_t rd;
    uint8_t ra;
    uint8_t z;
    dns_rcode_t rcode;
} dns_header_t;

typedef struct {
    uint8_t* name;
    dns_type_t type;
    dns_class_t class;
} dns_question_t;

typedef struct {
    uint8_t* name;
    dns_type_t type;
    dns_class_t class;
    uint32_t ttl;
    uint16_t rdlength;
    uint8_t* rdata;
} dns_rr_t;

typedef struct {
    dns_header_t header;
    dns_question_t question;
    dns_rr_t answer;
    dns_rr_t authority;
    dns_rr_t additional;
} dns_message_t;

HParser* dns_header_parser() {
    HParser* id_parser = h_int16(NULL);
    HParser* qr_parser = h_int8(NULL);
    HParser* opcode_parser = h_int8(NULL);
    HParser* aa_parser = h_int8(NULL);
    HParser* tc_parser = h_int8(NULL);
    HParser* rd_parser = h_int8(NULL);
    HParser* ra_parser = h_int8(NULL);
    HParser* z_parser = h_int8(NULL);
    HParser* rcode_parser = h_int8(NULL);

    HParser* header_parser = h_struct(
        h_field(id_parser, "id"),
        h_field(qr_parser, "qr"),
        h_field(opcode_parser, "opcode"),
        h_field(aa_parser, "aa"),
        h_field(tc_parser, "tc"),
        h_field(rd_parser, "rd"),
        h_field(ra_parser, "ra"),
        h_field(z_parser, "z"),
        h_field(rcode_parser, "rcode"),
        NULL
    );

    return header_parser;
}

HParser* dns_label_parser() {
    HParser* len_parser = h_int8(NULL);
    HParser* data_parser = h_bytes(len_parser);

    HParser* label_parser = h_struct(
        h_field(len_parser, "len"),
        h_field(data_parser, "data"),
        NULL
    );

    return label_parser;
}

HParser* dns_name_parser() {
    HParser* label_parser = dns_label_parser();
    HParser* name_parser = h_array(label_parser);

    return name_parser;
}

HParser* dns_question_parser() {
    HParser* name_parser = dns_name_parser();
    HParser* type_parser = h_int16(NULL);
    HParser* class_parser = h_int16(NULL);

    HParser* question_parser = h_struct(
        h_field(name_parser, "name"),
        h_field(type_parser, "type"),
        h_field(class_parser, "class"),
        NULL
    );

    return question_parser;
}

HParser* dns_rr_parser() {
    HParser* name_parser = dns_name_parser();
    HParser* type_parser = h_int16(NULL);
    HParser* class_parser = h_int16(NULL);
    HParser* ttl_parser = h_int32(NULL);
    HParser* rdlength_parser = h_int16(NULL);
    HParser* rdata_parser = h_bytes(rdlength_parser);

    HParser* rr_parser = h_struct(
        h_field(name_parser, "name"),
        h_field(type_parser, "type"),
        h_field(class_parser, "class"),
        h_field(ttl_parser, "ttl"),
        h_field(rdlength_parser, "rdlength"),
        h_field(rdata_parser, "rdata"),
        NULL
    );

    return rr_parser;
}

HParser* dns_message_parser() {
    HParser* header_parser = dns_header_parser();
    HParser* question_parser = dns_question_parser();
    HParser* answer_parser = dns_rr_parser();
    HParser* authority_parser = dns_rr_parser();
    HParser* additional_parser = dns_rr_parser();

    HParser* message_parser = h_struct(
        h_field(header_parser, "header"),
        h_field(question_parser, "question"),
        h_field(answer_parser, "answer"),
        h_field(authority_parser, "authority"),
        h_field(additional_parser, "additional"),
        NULL
    );

    return message_parser;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Failed to open file: %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    rewind(file);

    uint8_t* data = malloc(size);
    if (!data) {
        printf("Failed to allocate memory\n");
        return 1;
    }

    if (fread(data, 1, size, file) != size) {
        printf("Failed to read file\n");
        free(data);
        return 1;
    }

    fclose(file);

    HParser* parser = dns_message_parser();
    HParseResult* result = h_parse(parser, data, size);

    if (result->status == H_OK) {
        dns_header_t* header = (dns_header_t*)result->data;
        dns_question_t* question = (dns_question_t*)((uint8_t*)result->data + sizeof(dns_header_t));
        dns_rr_t* answer = (dns_rr_t*)((uint8_t*)result->data + sizeof(dns_header_t) + sizeof(dns_question_t));
        dns_rr_t* authority = (dns_rr_t*)((uint8_t*)result->data + sizeof(dns_header_t) + sizeof(dns_question_t) + sizeof(dns_rr_t));
        dns_rr_t* additional = (dns_rr_t*)((uint8_t*)result->data + sizeof(dns_header_t) + sizeof(dns_question_t) + sizeof(dns_rr_t) * 2);

        printf("ID: %u\n", header->id);
        printf("QR: %u\n", header->qr);
        printf("Opcode: %u\n", header->opcode);
        printf("AA: %u\n", header->aa);
        printf("TC: %u\n", header->tc);
        printf("RD: %u\n", header->rd);
        printf("RA: %u\n", header->ra);
        printf("Z: %u\n", header->z);
        printf("Rcode: %u\n", header->rcode);

        printf("Question name: ");
        for (size_t i = 0; i < strlen((char*)question->name); i++) {
            printf("%c", question->name[i]);
        }
        printf("\n");

        printf("Question type: %u\n", question->type);
        printf("Question class: %u\n", question->class);

        printf("Answer name: ");
        for (size_t i = 0; i < strlen((char*)answer->name); i++) {
            printf("%c", answer->name[i]);
        }
        printf("\n");

        printf("Answer type: %u\n", answer->type);
        printf("Answer class: %u\n", answer->class);
        printf("Answer TTL: %u\n", answer->ttl);
        printf("Answer RDLENGTH: %u\n", answer->rdlength);
        printf("Answer RDATA: ");
        for (size_t i = 0; i < answer->rdlength; i++) {
            printf("%02x", answer->rdata[i]);
        }
        printf("\n");

        printf("Authority name: ");
        for (size_t i = 0; i < strlen((char*)authority->name); i++) {
            printf("%c", authority->name[i]);
        }
        printf("\n");

        printf("Authority type: %u\n", authority->type);
        printf("Authority class: %u\n", authority->class);
        printf("Authority TTL: %u\n", authority->ttl);
        printf("Authority RDLENGTH: %u\n", authority->rdlength);
        printf("Authority RDATA: ");
        for (size_t i = 0; i < authority->rdlength; i++) {
            printf("%02x", authority->rdata[i]);
        }
        printf("\n");

        printf("Additional name: ");
        for (size_t i = 0; i < strlen((char*)additional->name); i++) {
            printf("%c", additional->name[i]);
        }
        printf("\n");

        printf("Additional type: %u\n", additional->type);
        printf("Additional class: %u\n", additional->class);
        printf("Additional TTL: %u\n", additional->ttl);
        printf("Additional RDLENGTH: %u\n", additional->rdlength);
        printf("Additional RDATA: ");
        for (size_t i = 0; i < additional->rdlength; i++) {
            printf("%02x", additional->rdata[i]);
        }
        printf("\n");
    }

    free(data);
    return 0;
}