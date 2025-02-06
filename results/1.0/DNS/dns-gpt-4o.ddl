@dns_message
{
    uint16 id;

    struct flags
    {
        uint4 qr;
        uint4 opcode;
        uint1 aa;
        uint1 tc;
        uint1 rd;
        uint1 ra;
        uint3 z;
        uint4 rcode;
    } flags;

    uint16 qdcount;
    uint16 ancount;
    uint16 nscount;
    uint16 arcount;

    question_section question[qdcount];
    resource_record answer[ancount];
    resource_record authority[nscount];
    resource_record additional[arcount];
}

@question_section
{
    domain_name qname;
    uint16 qtype;
    uint16 qclass;
}

@resource_record
{
    domain_name name;
    uint16 type;
    uint16 class;
    uint32 ttl;
    uint16 rdlength;
    uint8 rdata[rdlength];
}

@domain_name
{
    while (1) {
        uint8 len;
        if (len == 0) break;
        uint8 label[len];
    }
}