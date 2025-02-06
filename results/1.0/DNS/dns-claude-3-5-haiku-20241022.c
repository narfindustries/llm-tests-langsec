#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

typedef struct {
    uint16_t transaction_id;
    struct {
        uint8_t qr: 1;
        uint8_t opcode: 4;
        uint8_t aa: 1;
        uint8_t tc: 1;
        uint8_t rd: 1;
        uint8_t ra: 1;
        uint8_t z: 3;
        uint8_t rcode: 4;
    } flags;
    uint16_t question_count;
    uint16_t answer_count;
    uint16_t authority_count;
    uint16_t additional_count;
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

static HParsedToken* dns_header_action(const HParseResult* result, void* user_data) {
    return result->ast;
}

static HParsedToken* dns_name_action(const HParseResult* result, void* user_data) {
    return result->ast;
}

static HParsedToken* dns_question_action(const HParseResult* result, void* user_data) {
    return result->ast;
}

static HParsedToken* dns_resource_record_action(const HParseResult* result, void* user_data) {
    return result->ast;
}

HParser* make_dns_header_parser(void) {
    HParser* transaction_id = h_uint16();
    HParser* flags = h_bits(16, false);
    HParser* counts = h_repeat_n(h_uint16(), 4);

    HParser* dns_header = h_sequence(transaction_id, flags, counts, NULL);
    return h_action(dns_header, dns_header_action, NULL);
}

HParser* make_dns_name_parser(void) {
    HParser* label_length = h_uint8();
    HParser* label = h_repeat_n(h_uint8(), 255);
    HParser* name = h_many(label);

    return h_action(name, dns_name_action, NULL);
}

HParser* make_dns_question_parser(void) {
    HParser* name = make_dns_name_parser();
    HParser* type = h_uint16();
    HParser* class = h_uint16();

    HParser* dns_question = h_sequence(name, type, class, NULL);
    return h_action(dns_question, dns_question_action, NULL);
}

HParser* make_dns_resource_record_parser(void) {
    HParser* name = make_dns_name_parser();
    HParser* type = h_uint16();
    HParser* class = h_uint16();
    HParser* ttl = h_uint32();
    HParser* rdlength = h_uint16();
    HParser* rdata = h_repeat_n(h_uint8(), 65535);

    HParser* resource_record = h_sequence(name, type, class, ttl, rdlength, rdata, NULL);
    return h_action(resource_record, dns_resource_record_action, NULL);
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
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
        perror("Memory allocation failed");
        fclose(file);
        return 1;
    }

    size_t bytes_read = fread(buffer, 1, file_size, file);
    fclose(file);

    if (bytes_read != file_size) {
        perror("Error reading file");
        free(buffer);
        return 1;
    }

    HParser* header_parser = make_dns_header_parser();
    HParseResult* header = h_parse(header_parser, buffer, file_size);
    
    if (header && header->ast) {
        printf("DNS Header parsed successfully\n");
        h_parse_result_free(header);
    }

    free(buffer);
    return 0;
}