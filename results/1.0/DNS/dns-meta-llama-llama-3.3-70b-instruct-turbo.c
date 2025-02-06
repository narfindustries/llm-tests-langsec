#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    unsigned short x;
} id;

typedef struct {
    unsigned char x;
} qr;

typedef struct {
    unsigned char x;
} opcode;

typedef struct {
    unsigned char x;
} aa;

typedef struct {
    unsigned char x;
} tc;

typedef struct {
    unsigned char x;
} rd;

typedef struct {
    unsigned char x;
} ra;

typedef struct {
    unsigned char x;
} z;

typedef struct {
    unsigned char x;
} rcode;

typedef struct {
    unsigned char* x;
    unsigned short len;
} qname;

typedef struct {
    unsigned short x;
} qtype;

typedef struct {
    unsigned short x;
} qclass;

typedef struct {
    unsigned char* x;
    unsigned short len;
} anname;

typedef struct {
    unsigned short x;
} antype;

typedef struct {
    unsigned short x;
} anclass;

typedef struct {
    unsigned int x;
} an_ttl;

typedef struct {
    unsigned short x;
} an_rdlength;

typedef struct {
    unsigned char* x;
    unsigned short len;
} an_rdata;

typedef struct {
    unsigned char* x;
    unsigned short len;
} nsname;

typedef struct {
    unsigned short x;
} nstype;

typedef struct {
    unsigned short x;
} nsclass;

typedef struct {
    unsigned int x;
} ns_ttl;

typedef struct {
    unsigned short x;
} ns_rdlength;

typedef struct {
    unsigned char* x;
    unsigned short len;
} ns_rdata;

typedef struct {
    unsigned char* x;
    unsigned short len;
} arname;

typedef struct {
    unsigned short x;
} artype;

typedef struct {
    unsigned short x;
} arclass;

typedef struct {
    unsigned int x;
} ar_ttl;

typedef struct {
    unsigned short x;
} ar_rdlength;

typedef struct {
    unsigned char* x;
    unsigned short len;
} ar_rdata;

typedef struct {
    id id;
    struct {
        qr qr;
        opcode opcode;
        aa aa;
        tc tc;
        rd rd;
        ra ra;
        z z;
        rcode rcode;
    } flags;
} header;

typedef struct {
    qname qname;
    qtype qtype;
    qclass qclass;
} question;

typedef struct {
    anname anname;
    antype antype;
    anclass anclass;
    an_ttl an_ttl;
    an_rdlength an_rdlength;
    an_rdata an_rdata;
} answer;

typedef struct {
    nsname nsname;
    nstype nstype;
    nsclass nsclass;
    ns_ttl ns_ttl;
    ns_rdlength ns_rdlength;
    ns_rdata ns_rdata;
} authority;

typedef struct {
    arname arname;
    artype artype;
    arclass arclass;
    ar_ttl ar_ttl;
    ar_rdlength ar_rdlength;
    ar_rdata ar_rdata;
} additional;

typedef struct {
    header header;
    struct {
        question* x;
        unsigned short len;
    } questions;
    struct {
        answer* x;
        unsigned short len;
    } answers;
    struct {
        authority* x;
        unsigned short len;
    } authorities;
    struct {
        additional* x;
        unsigned short len;
    } additionals;
} dns_message;

void headerhammer(hammer_t* h, header* s) {
    hammer_bseq(h, &s->id.x, 2);
    hammer_bseq(h, &s->flags.qr.x, 1);
    hammer_bseq(h, &s->flags.opcode.x, 1);
    hammer_bseq(h, &s->flags.aa.x, 1);
    hammer_bseq(h, &s->flags.tc.x, 1);
    hammer_bseq(h, &s->flags.rd.x, 1);
    hammer_bseq(h, &s->flags.ra.x, 1);
    hammer_bseq(h, &s->flags.z.x, 1);
    hammer_bseq(h, &s->flags.rcode.x, 1);
}

void qnamehammer(hammer_t* h, qname* s) {
    unsigned short i;
    hammer_u16(h, &s->len);
    s->x = malloc(s->len);
    for (i = 0; i < s->len; i++)
        hammer_bseq(h, &s->x[i], 1);
}

void questionhammer(hammer_t* h, question* s) {
    headerhammer(h, NULL);
    qnamehammer(h, &s->qname);
    hammer_u16(h, &s->qtype.x);
    hammer_u16(h, &s->qclass.x);
}

void answerhammer(hammer_t* h, answer* s) {
    qnamehammer(h, &s->anname);
    hammer_u16(h, &s->antype.x);
    hammer_u16(h, &s->anclass.x);
    hammer_u32(h, &s->an_ttl.x);
    hammer_u16(h, &s->an_rdlength.x);
    s->an_rdata.x = malloc(s->an_rdlength.x);
    hammer_bseq(h, s->an_rdata.x, s->an_rdlength.x);
    s->an_rdata.len = s->an_rdlength.x;
}

void authorityhammer(hammer_t* h, authority* s) {
    qnamehammer(h, &s->nsname);
    hammer_u16(h, &s->nstype.x);
    hammer_u16(h, &s->nsclass.x);
    hammer_u32(h, &s->ns_ttl.x);
    hammer_u16(h, &s->ns_rdlength.x);
    s->ns_rdata.x = malloc(s->ns_rdlength.x);
    hammer_bseq(h, s->ns_rdata.x, s->ns_rdlength.x);
    s->ns_rdata.len = s->ns_rdlength.x;
}

void additionalhammer(hammer_t* h, additional* s) {
    qnamehammer(h, &s->arname);
    hammer_u16(h, &s->artype.x);
    hammer_u16(h, &s->arclass.x);
    hammer_u32(h, &s->ar_ttl.x);
    hammer_u16(h, &s->ar_rdlength.x);
    s->ar_rdata.x = malloc(s->ar_rdlength.x);
    hammer_bseq(h, s->ar_rdata.x, s->ar_rdlength.x);
    s->ar_rdata.len = s->ar_rdlength.x;
}

void dns_messagehammer(hammer_t* h, dns_message* s) {
    headerhammer(h, &s->header);
    hammer_u16(h, &s->questions.len);
    s->questions.x = malloc(s->questions.len * sizeof(question));
    unsigned short i;
    for (i = 0; i < s->questions.len; i++)
        questionhammer(h, &s->questions.x[i]);
    hammer_u16(h, &s->answers.len);
    s->answers.x = malloc(s->answers.len * sizeof(answer));
    for (i = 0; i < s->answers.len; i++)
        answerhammer(h, &s->answers.x[i]);
    hammer_u16(h, &s->authorities.len);
    s->authorities.x = malloc(s->authorities.len * sizeof(authority));
    for (i = 0; i < s->authorities.len; i++)
        authorityhammer(h, &s->authorities.x[i]);
    hammer_u16(h, &s->additionals.len);
    s->additionals.x = malloc(s->additionals.len * sizeof(additional));
    for (i = 0; i < s->additionals.len; i++)
        additionalhammer(h, &s->additionals.x[i]);
}

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    dns_message msg;
    FILE* file = fopen(argv[1], "rb");
    hammer_t hammer;
    hammer_init(&hammer, file);
    dns_messagehammer(&hammer, &msg);
    hammer_free(&hammer);
    fclose(file);

    return 0;
}