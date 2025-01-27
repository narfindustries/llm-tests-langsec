#include <hammer/hammer.h>
#include <hammer/glue.h>

// Forward declarations for recursive structures
static HParsedToken *act_DNSQuestion(const HParseResult *p, void *user_data);
static HParsedToken *act_DNSRecord(const HParseResult *p, void *user_data);

// Basic DNS types
static HParser *dns_name;
static HParser *dns_type;
static HParser *dns_class;
static HParser *dns_ttl;
static HParser *dns_rdlength;
static HParser *dns_rdata;

// DNS message sections
static HParser *dns_question;
static HParser *dns_record;
static HParser *dns_questions;
static HParser *dns_answers;
static HParser *dns_authorities;
static HParser *dns_additionals;

// DNS header fields
static HParser *dns_id;
static HParser *dns_flags;
static HParser *dns_qdcount;
static HParser *dns_ancount;
static HParser *dns_nscount;
static HParser *dns_arcount;

// Complete DNS message
static HParser *dns_message;

// Helper parsers
static HParser *uint16;
static HParser *uint32;

// Actions to convert parsed data into a more usable form
static HParsedToken *act_DNSName(const HParseResult *p, void *user_data) {
    return H_MAKE_STR(strndup(p->bit_offset, p->bit_length));
}

static void init_parsers() {
    uint16 = h_uint16();
    uint32 = h_uint32();

    dns_name = h_action(h_repeat_n(h_choice(h_ch_range(0x01, 0x2f), NULL), h_uint8(), NULL), act_DNSName, NULL);
    dns_type = uint16;
    dns_class = uint16;
    dns_ttl = uint32;
    dns_rdlength = uint16;
    dns_rdata = h_length_value(dns_rdlength, h_arbitrary_bytes());

    dns_question = h_sequence(dns_name, dns_type, dns_class, NULL);
    dns_record = h_sequence(dns_name, dns_type, dns_class, dns_ttl, dns_rdlength, dns_rdata, NULL);

    dns_questions = h_repeat_n(dns_question, dns_qdcount);
    dns_answers = h_repeat_n(dns_record, dns_ancount);
    dns_authorities = h_repeat_n(dns_record, dns_nscount);
    dns_additionals = h_repeat_n(dns_record, dns_arcount);

    dns_id = uint16;
    dns_flags = uint16;
    dns_qdcount = uint16;
    dns_ancount = uint16;
    dns_nscount = uint16;
    dns_arcount = uint16;

    dns_message = h_sequence(
        dns_id, dns_flags, dns_qdcount, dns_ancount, dns_nscount, dns_arcount,
        dns_questions, dns_answers, dns_authorities, dns_additionals,
        NULL
    );
}

int main(int argc, char **argv) {
    HParser *parser;
    init_parsers();
    parser = dns_message;

    // Assuming input is provided as a binary file for simplicity
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_dns_data_file>\n", argv[0]);
        return 1;
    }

    // Read the binary file
    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t *data = malloc(size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(f);
        return 1;
    }

    if (fread(data, 1, size, f) != size) {
        perror("Failed to read file");
        free(data);
        fclose(f);
        return 1;
    }

    fclose(f);

    // Parse the DNS data
    HParseResult *result = h_parse(parser, data, size * 8);
    if (!result) {
        fprintf(stderr, "Parsing failed.\n");
        free(data);
        return 1;
    }

    // Output or process the parse result
    h_pprint(stdout, result->ast, 0, 0);

    // Cleanup
    h_parse_result_free(result);
    free(data);
    return 0;
}