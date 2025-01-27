#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

HParser *create_dns_parser() {
    // Define basic parsers
    HParser *label_length = h_uint8();
    HParser *label = h_repeat_n(h_uint8(), label_length);
    HParser *domain_name = h_many1(h_sequence(label, h_ch('.'), NULL));

    // Define DNS header fields
    HParser *dns_id = h_uint16();
    HParser *dns_flags = h_uint16();
    HParser *dns_qdcount = h_uint16();
    HParser *dns_ancount = h_uint16();
    HParser *dns_nscount = h_uint16();
    HParser *dns_arcount = h_uint16();

    // Define DNS question fields
    HParser *dns_qname = domain_name;
    HParser *dns_qtype = h_uint16();
    HParser *dns_qclass = h_uint16();

    // Define DNS question section
    HParser *dns_question = h_sequence(dns_qname, dns_qtype, dns_qclass, NULL);

    // Define DNS record fields
    HParser *dns_rname = domain_name;
    HParser *dns_rtype = h_uint16();
    HParser *dns_rclass = h_uint16();
    HParser *dns_rttl = h_uint32();
    HParser *dns_rdlength = h_uint16();
    HParser *dns_rdata = h_repeat_n(h_uint8(), dns_rdlength);

    // Define DNS record section
    HParser *dns_record = h_sequence(dns_rname, dns_rtype, dns_rclass, dns_rttl, dns_rdlength, dns_rdata, NULL);

    // Define complete DNS message parser
    HParser *dns_parser = h_sequence(
        dns_id,
        dns_flags,
        dns_qdcount,
        dns_ancount,
        dns_nscount,
        dns_arcount,
        h_repeat_n(dns_question, dns_qdcount),
        h_repeat_n(dns_record, dns_ancount),
        h_repeat_n(dns_record, dns_nscount),
        h_repeat_n(dns_record, dns_arcount),
        NULL
    );

    return dns_parser;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <dns_packet_file>\n", argv[0]);
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

    unsigned char *buffer = (unsigned char *)malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *dns_parser = create_dns_parser();
    HParseResult *result = h_parse(dns_parser, buffer, file_size);

    if (result) {
        printf("DNS packet parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse DNS packet.\n");
    }

    free(buffer);
    h_parser_free(dns_parser);

    return EXIT_SUCCESS;
}