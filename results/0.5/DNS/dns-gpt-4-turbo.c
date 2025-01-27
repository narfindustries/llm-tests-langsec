#include <hammer/hammer.h>
#include <hammer/glue.h>

// Forward declarations
static HParsedToken *act_DomainName(const HParseResult *p, void *user_data);
static HParsedToken *act_DnsQuestion(const HParseResult *p, void *user_data);
static HParsedToken *act_DnsResourceRecord(const HParseResult *p, void *user_data);
static HParsedToken *act_DnsMessage(const HParseResult *p, void *user_data);

// Basic types
static HParser *h_uint8 = h_uint8();
static HParser *h_uint16 = h_uint16be();
static HParser *h_uint32 = h_uint32be();

// DNS Label
static HParser *DNS_label;
static HParser *DNS_labels;

// DNS Domain Name
static HParser *DNS_domain_name;

// DNS Question Section
static HParser *DNS_question;

// DNS Resource Record
static HParser *DNS_resource_record;

// DNS Message
static HParser *DNS_message;

// Label parser
static HParser *DNS_label = h_action(h_length_value(h_uint8(), h_uint8()), act_DomainName, NULL);

// Labels sequence parser
static HParser *DNS_labels = h_many1(DNS_label);

// Domain name parser
static HParser *DNS_domain_name = h_right(h_ch('\0'), DNS_labels);

// DNS Question section
static HParser *DNS_question = h_action(h_sequence(DNS_domain_name, h_uint16(), h_uint16(), NULL), act_DnsQuestion, NULL);

// DNS Resource Record
static HParser *DNS_resource_record = h_action(h_sequence(DNS_domain_name, h_uint16(), h_uint16(), h_uint32(), h_length_value(h_uint16(), h_uint8()), NULL), act_DnsResourceRecord, NULL);

// DNS Message
static HParser *DNS_message = h_action(h_sequence(h_uint16(), h_uint16(), h_uint16(), h_uint16(), h_uint16(), h_uint16(),
                                                  h_many(DNS_question),
                                                  h_many(DNS_resource_record),
                                                  h_many(DNS_resource_record),
                                                  h_many(DNS_resource_record), NULL), act_DnsMessage, NULL);

// Actions
static HParsedToken *act_DomainName(const HParseResult *p, void *user_data) {
    return H_MAKE_STR(p->ast->token);
}

static HParsedToken *act_DnsQuestion(const HParseResult *p, void *user_data) {
    return H_MAKE_SEQ((HParsedToken *)p->ast->seq->elements[0], 
                      H_MAKE_UINT(p->ast->seq->elements[1]->uint),
                      H_MAKE_UINT(p->ast->seq->elements[2]->uint));
}

static HParsedToken *act_DnsResourceRecord(const HParseResult *p, void *user_data) {
    return H_MAKE_SEQ((HParsedToken *)p->ast->seq->elements[0], 
                      H_MAKE_UINT(p->ast->seq->elements[1]->uint),
                      H_MAKE_UINT(p->ast->seq->elements[2]->uint),
                      H_MAKE_UINT(p->ast->seq->elements[3]->uint),
                      H_MAKE_BYTES(p->ast->seq->elements[4]->seq->elements, p->ast->seq->elements[4]->seq->used));
}

static HParsedToken *act_DnsMessage(const HParseResult *p, void *user_data) {
    return H_MAKE_SEQ(H_MAKE_UINT(p->ast->seq->elements[0]->uint),
                      H_MAKE_UINT(p->ast->seq->elements[1]->uint),
                      H_MAKE_UINT(p->ast->seq->elements[2]->uint),
                      H_MAKE_UINT(p->ast->seq->elements[3]->uint),
                      H_MAKE_UINT(p->ast->seq->elements[4]->uint),
                      H_MAKE_UINT(p->ast->seq->elements[5]->uint),
                      (HParsedToken *)p->ast->seq->elements[6],
                      (HParsedToken *)p->ast->seq->elements[7],
                      (HParsedToken *)p->ast->seq->elements[8],
                      (HParsedToken *)p->ast->seq->elements[9]);
}

// Main
int main(int argc, char **argv) {
    HParser *parser = DNS_message;
    HParseResult *result = h_parse(parser, (const uint8_t *)"\x03www\x06google\x03com\x00\x00\x01\x00\x01", 16);
    if (result) {
        h_pprint(stdout, result->ast, 0, 0);
        h_parse_result_free(result);
    }
    return 0;
}