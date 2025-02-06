#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

static HParser *dns_id;
static HParser *dns_qr;
static HParser *dns_opcode;
static HParser *dns_aa;
static HParser *dns_tc;
static HParser *dns_rd;
static HParser *dns_ra;
static HParser *dns_z;
static HParser *dns_rcode;
static HParser *dns_qdcount;
static HParser *dns_ancount;
static HParser *dns_nscount;
static HParser *dns_arcount;
static HParser *dns_qtype;
static HParser *dns_qclass;
static HParser *dns_question;
static HParser *dns_ttl;
static HParser *dns_rdlength;
static HParser *dns_rdata;
static HParser *dns_rr;
static HParser *dns_header;
static HParser *dns_message;

static void init_parsers(void) {
    dns_id = h_uint16();
    dns_qr = h_bits(1, false);
    dns_opcode = h_bits(4, false);
    dns_aa = h_bits(1, false);
    dns_tc = h_bits(1, false);
    dns_rd = h_bits(1, false);
    dns_ra = h_bits(1, false);
    dns_z = h_bits(3, false);
    dns_rcode = h_bits(4, false);
    dns_qdcount = h_uint16();
    dns_ancount = h_uint16();
    dns_nscount = h_uint16();
    dns_arcount = h_uint16();
    
    HParser *label_length = h_uint8();
    HParser *label_chars = h_repeat_n(h_ch_range(0x20, 0x7E), 1);
    HParser *label = h_sequence(label_length, label_chars, NULL);
    HParser *name = h_many(label);
    
    dns_qtype = h_uint16();
    dns_qclass = h_uint16();
    dns_question = h_sequence(name, dns_qtype, dns_qclass, NULL);
    
    dns_ttl = h_uint32();
    dns_rdlength = h_uint16();
    dns_rdata = h_length_value(dns_rdlength, h_uint8());
    
    dns_rr = h_sequence(
        name,
        dns_qtype,
        dns_qclass,
        dns_ttl,
        dns_rdlength,
        dns_rdata,
        NULL
    );
    
    dns_header = h_sequence(
        dns_id,
        dns_qr,
        dns_opcode,
        dns_aa,
        dns_tc,
        dns_rd,
        dns_ra,
        dns_z,
        dns_rcode,
        dns_qdcount,
        dns_ancount,
        dns_nscount,
        dns_arcount,
        NULL
    );
    
    dns_message = h_sequence(
        dns_header,
        h_many(dns_question),
        h_many(dns_rr),  // Answer section
        h_many(dns_rr),  // Authority section
        h_many(dns_rr),  // Additional section
        NULL
    );
}

void print_parse_result(const HParsedToken* result) {
    if (!result) {
        printf("Parse failed\n");
        return;
    }
    printf("Successfully parsed DNS message\n");
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <dns_binary_file>\n", argv[0]);
        return 1;
    }

    init_parsers();

    FILE* f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t* buffer = malloc(size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(f);
        return 1;
    }

    if (fread(buffer, 1, size, f) != size) {
        perror("Failed to read file");
        free(buffer);
        fclose(f);
        return 1;
    }

    const HParseResult* result = h_parse(dns_message, buffer, size);
    if (result) {
        print_parse_result(result->ast);
        h_parse_result_free((HParseResult*)result);
    } else {
        printf("Failed to parse DNS message\n");
    }

    free(buffer);
    fclose(f);
    return 0;
}