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
    uint16_t type;
    uint16_t class;
} dns_question_t;

typedef struct {
    uint16_t type;
    uint16_t class;
    uint32_t ttl;
    uint16_t rdlength;
    uint8_t *rdata;
} dns_resource_record_t;

hm_parser_t* parse_domain_name(void);
hm_parser_t* parse_rdata(uint16_t type);


int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <dns_binary_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buffer = malloc(fsize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }

    fread(buffer, 1, fsize, fp);
    fclose(fp);

    hm_parser_t* header_parser = hm_seq(
        hm_uint16("id"),
        hm_uint16("flags"),
        hm_uint16("qdcount"),
        hm_uint16("ancount"),
        hm_uint16("nscount"),
        hm_uint16("arcount"),
        NULL
    );

    hm_parser_t* question_parser = hm_seq(
        parse_domain_name(),
        hm_uint16("qtype"),
        hm_uint16("qclass"),
        NULL
    );

    hm_parser_t* rdata_parser = hm_choice(
        hm_uint32("rdata_ipv4"),
        hm_uint128("rdata_ipv6"),
        NULL
    );

    hm_parser_t* rr_parser = hm_seq(
        parse_domain_name(),
        hm_uint16("type"),
        hm_uint16("class"),
        hm_uint32("ttl"),
        hm_uint16("rdlength"),
        hm_cast(hm_bytes("rdata", hm_ref("rdlength")), hm_uint8_p("rdata")),
        NULL
    );


    hm_parser_t* dns_parser = hm_seq(
        header_parser,
        hm_count("questions", question_parser),
        hm_count("answers", rr_parser),
        hm_count("ns", rr_parser),
        hm_count("ar", rr_parser),
        NULL
    );

    hm_result_t result = hm_parse(dns_parser, buffer, fsize);

    if (result.status == HM_SUCCESS) {
        printf("DNS message parsed successfully!\n");
    } else {
        fprintf(stderr, "Error parsing DNS message: %s\n", hm_error_message(result.status));
    }

    free(buffer);
    return 0;
}


hm_parser_t* parse_domain_name(void) {
    hm_parser_t* label_parser = hm_seq(
        hm_uint8("len"),
        hm_bytes("label", hm_ref("len")),
        NULL
    );
    hm_parser_t* domain_parser = hm_many1(label_parser);
    return domain_parser;
}

hm_parser_t* parse_rdata(uint16_t type) {
    switch (type) {
        case 1:
            return hm_uint32("ipv4");
        case 28:
            return hm_uint128("ipv6");
        default:
            return hm_bytes("rdata", hm_uint16("rdlength"));
    }
}
