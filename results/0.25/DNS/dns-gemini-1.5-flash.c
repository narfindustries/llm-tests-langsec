#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <arpa/inet.h>
#include <string.h>

typedef uint8_t byte;

// Helper function to parse a domain name label
static HParser* dns_label_parser = map_parser(length_prefixed_parser(uint8_t_parser), (HMapFunc)strlen);

// Helper function to parse a domain name
static HParser* dns_name_parser = recursive_parser(
    choice_parser(
        map_parser(bytes_parser(0), (HMapFunc)strdup),
        sequence_parser(
            dns_label_parser,
            recursive_parser(
                choice_parser(
                    map_parser(bytes_parser(0), (HMapFunc)strdup),
                    sequence_parser(
                        uint8_t_parser,
                        dns_label_parser
                    )
                )
            )
        )
    )
);

//Helper function to parse resource data based on type.  This is a placeholder and needs significant expansion for a real DNS parser.
static HParser* dns_rdata_parser = any_parser;


// Define Hammer parsers for DNS header fields
static HParser* dns_id = uint16_t_parser;
static HParser* dns_qr = bit_parser;
static HParser* dns_opcode = bits_parser(4);
static HParser* dns_aa = bit_parser;
static HParser* dns_tc = bit_parser;
static HParser* dns_rd = bit_parser;
static HParser* dns_ra = bit_parser;
static HParser* dns_z = bits_parser(3);
static HParser* dns_rcode = bits_parser(4);
static HParser* dns_qdcount = uint16_t_parser;
static HParser* dns_ancount = uint16_t_parser;
static HParser* dns_nscount = uint16_t_parser;
static HParser* dns_arcount = uint16_t_parser;

// Define Hammer parser for DNS header
static HParser* dns_header_parser = sequence_parser(dns_id, dns_qr, dns_opcode, dns_aa, dns_tc, dns_rd, dns_ra, dns_z, dns_rcode, dns_qdcount, dns_ancount, dns_nscount, dns_arcount);

// Define Hammer parsers for DNS question section
static HParser* dns_qtype = uint16_t_parser;
static HParser* dns_qclass = uint16_t_parser;
static HParser* dns_question = sequence_parser(dns_name_parser, dns_qtype, dns_qclass);

// Define Hammer parsers for DNS resource record
static HParser* dns_rr_type = uint16_t_parser;
static HParser* dns_rr_class = uint16_t_parser;
static HParser* dns_rr_ttl = uint32_t_parser;
static HParser* dns_rr_rdlength = uint16_t_parser;
static HParser* dns_rr_rdata = dns_rdata_parser;
static HParser* dns_resource_record = sequence_parser(dns_name_parser, dns_rr_type, dns_rr_class, dns_rr_ttl, dns_rr_rdlength, dns_rr_rdata);

// Define Hammer parser for DNS message
static HParser* dns_message_parser = sequence_parser(dns_header_parser, repeat_parser(dns_question), repeat_parser(dns_resource_record), repeat_parser(dns_resource_record), repeat_parser(dns_resource_record));

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE* fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fileSize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char* buffer = (char*)malloc(fileSize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }
    fread(buffer, 1, fileSize, fp);
    fclose(fp);

    HResult result = hammer_parse(dns_message_parser, buffer, fileSize);

    if (result.success) {
        printf("DNS message parsed successfully!\n");
        // Access parsed data here using result.value
    } else {
        fprintf(stderr, "Error parsing DNS message: %s\n", result.error);
    }

    free(buffer);
    return 0;
}
