#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *dns_label() {
    return h_many1(h_choice(h_uint8(), h_ch(0), NULL));
}

HParser *dns_name() {
    return h_many1(dns_label());
}

HParser *dns_question() {
    return h_sequence(dns_name(), h_uint16(), h_uint16(), NULL);
}

HParser *dns_rr() {
    return h_sequence(dns_name(),
                      h_uint16(), // TYPE
                      h_uint16(), // CLASS
                      h_uint32(), // TTL
                      h_length_value(h_uint16(), h_data(h_uint16())), // RDLENGTH and RDATA
                      NULL);
}

HParser *dns_header() {
    return h_sequence(h_uint16(), // ID
                      h_bits(1, false),   // QR
                      h_bits(4, false),   // Opcode
                      h_bits(1, false),   // AA
                      h_bits(1, false),   // TC
                      h_bits(1, false),   // RD
                      h_bits(1, false),   // RA
                      h_bits(3, false),   // Z
                      h_bits(4, false),   // RCODE
                      h_uint16(), // QDCOUNT
                      h_uint16(), // ANCOUNT
                      h_uint16(), // NSCOUNT
                      h_uint16(), // ARCOUNT
                      NULL);
}

HParser *dns_message() {
    return h_action(h_sequence(
        dns_header(),
        h_many(dns_question()), // Question section
        h_many(dns_rr()),       // Answer section
        h_many(dns_rr()),       // Authority section
        h_many(dns_rr()),       // Additional section
        NULL), NULL);
}

void parse_dns_file(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }

    fseek(file, 0, SEEK_END);
    long filesize = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *data = malloc(filesize);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    fread(data, 1, filesize, file);
    fclose(file);

    HParser *parser = dns_message();
    HParseResult *result = h_parse(parser, data, filesize);

    if (result->ast == NULL) {
        fprintf(stderr, "Parse error\n");
    } else {
        printf("Parsing successful!\n");
    }

    h_parse_result_free(result);
    free(data);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    parse_dns_file(argv[1]);
    return EXIT_SUCCESS;
}