#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *create_dns_parser() {
    // Define basic parsers
    HParser *h_uint16_parser = h_uint16();
    HParser *h_uint32_parser = h_uint32();
    HParser *h_byte_parser = h_uint8();
    HParser *h_label = h_many1(h_choice(h_byte_parser, h_byte_parser));

    // Header fields
    HParser *id = h_uint16_parser;
    HParser *flags = h_bits(16, false);
    HParser *qdcount = h_uint16_parser;
    HParser *ancount = h_uint16_parser;
    HParser *nscount = h_uint16_parser;
    HParser *arcount = h_uint16_parser;

    // Question section
    HParser *qname = h_many1(h_label);
    HParser *qtype = h_uint16_parser;
    HParser *qclass = h_uint16_parser;

    // Resource Record fields
    HParser *name = h_many1(h_label);
    HParser *type = h_uint16_parser;
    HParser *class = h_uint16_parser;
    HParser *ttl = h_uint32_parser;
    HParser *rdlength = h_uint16_parser;
    HParser *rdata = h_length_value(rdlength, h_uint8());

    // Define the structure of a question
    HParser *question = h_sequence(qname, qtype, qclass, NULL);

    // Define the structure of a resource record
    HParser *resource_record = h_sequence(name, type, class, ttl, rdlength, rdata, NULL);

    // Define the DNS message structure
    HParser *dns_message = h_sequence(
        id, flags, qdcount, ancount, nscount, arcount,
        h_many(question),
        h_many(resource_record),
        h_many(resource_record),
        h_many(resource_record),
        NULL
    );

    return dns_message;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *dns_parser = create_dns_parser();
    HParseResult *result = h_parse(dns_parser, buffer, file_size);

    if (result->ast) {
        printf("DNS message parsed successfully.\n");
        h_ast_print(result->ast, stdout);
    } else {
        fprintf(stderr, "Failed to parse DNS message.\n");
    }

    h_parse_result_free(result);
    h_parser_free(dns_parser);
    free(buffer);

    return EXIT_SUCCESS;
}