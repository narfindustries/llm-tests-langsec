dns_message : struct {
    id : uint16;
    flags : bitfield {
        qr : uint16[1];
        opcode : uint16[4];
        aa : uint16[1];
        tc : uint16[1];
        rd : uint16[1];
        ra : uint16[1];
        z : uint16[3];
        rcode : uint16[4];
    };
    qdcount : uint16;
    ancount : uint16;
    nscount : uint16;
    arcount : uint16;

    questions : question[qdcount];
    answers : resource_record[ancount];
    authorities : resource_record[nscount];
    additionals : resource_record[arcount];
}

question : struct {
    qname : domain_name;
    qtype : uint16;
    qclass : uint16;
}

resource_record : struct {
    name : domain_name;
    type : uint16;
    class : uint16;
    ttl : uint32;
    rdlength : uint16;
    rdata : uint8[rdlength];
}

domain_name : list(domain_label);

domain_label : struct {
    length : uint8;
    label : uint8[length] if length > 0;
}