#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef struct {
    uint16_t transaction_id;
    struct {
        uint8_t qr : 1;
        uint8_t opcode : 4;
        uint8_t aa : 1;
        uint8_t tc : 1;
        uint8_t rd : 1;
        uint8_t ra : 1;
        uint8_t z : 3;
        uint8_t rcode : 4;
    } __attribute__((packed)) flags;
    uint16_t question_count;
    uint16_t answer_count;
    uint16_t authority_count;
    uint16_t additional_count;
} __attribute__((packed)) DNSHeader;

static HParser* parse_dns_header(void) {
    return h_sequence(
        h_uint16(),   // transaction_id
        h_bits(16, false),  // flags
        h_uint16(),   // question_count
        h_uint16(),   // answer_count
        h_uint16(),   // authority_count
        h_uint16(),   // additional_count
        NULL
    );
}

static HParser* parse_dns_name(void) {
    return h_sequence(
        h_many(
            h_sequence(
                h_uint8(),  // label length
                h_repeat_n(h_ch_range('a', 'z'), 63),  // label content
                NULL
            )
        ),
        h_ch(0),  // null terminator
        NULL
    );
}

static HParser* parse_dns_question(void) {
    return h_sequence(
        parse_dns_name(),  // name
        h_uint16(),  // type
        h_uint16(),  // class
        NULL
    );
}

static HParser* parse_dns_resource_record(void) {
    return h_sequence(
        parse_dns_name(),  // name
        h_uint16(),  // type
        h_uint16(),  // class
        h_uint32(),  // ttl
        h_length_value(h_uint16(), h_uint8()),  // rdlength and rdata
        NULL
    );
}

static HParser* parse_dns_message(void) {
    return h_sequence(
        parse_dns_header(),
        h_repeat_n(parse_dns_question(), 1),
        h_repeat_n(parse_dns_resource_record(), 1),
        h_repeat_n(parse_dns_resource_record(), 1),
        h_repeat_n(parse_dns_resource_record(), 1),
        NULL
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

    size_t read_size = fread(buffer, 1, file_size, file);
    if (read_size != file_size) {
        perror("File read error");
        free(buffer);
        fclose(file);
        return 1;
    }

    HParser* dns_parser = h_build_parser(parse_dns_message(), NULL);
    HParseResult* result = h_parse(dns_parser, buffer, file_size);

    if (result && result->ast) {
        printf("DNS message parsed successfully\n");
        h_parse_result_free(result);
        h_parser_free(dns_parser);
        free(buffer);
        fclose(file);
        return 0;
    } else {
        fprintf(stderr, "DNS message parsing failed\n");
        h_parse_result_free(result);
        h_parser_free(dns_parser);
        free(buffer);
        fclose(file);
        return 1;
    }
}