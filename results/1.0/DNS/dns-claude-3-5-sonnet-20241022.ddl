bitfield flags {
    qr: 1
    opcode: 4
    aa: 1
    tc: 1
    rd: 1
    ra: 1
    z: 3
    rcode: 4
}

enum Type : uint16 {
    A = 1
    NS = 2
    MD = 3
    MF = 4
    CNAME = 5
    SOA = 6
    MB = 7
    MG = 8
    MR = 9
    NULL = 10
    WKS = 11
    PTR = 12
    HINFO = 13
    MINFO = 14
    MX = 15
    TXT = 16
    AAAA = 28
    AXFR = 252
    ANY = 255
}

enum Class : uint16 {
    IN = 1
    CS = 2
    CH = 3
    HS = 4
    ANY = 255
}

format DNS_Message {
    header: DNS_Header
    questions: DNS_Question[header.qdcount]
    answers: ResourceRecord[header.ancount]
    authorities: ResourceRecord[header.nscount]
    additionals: ResourceRecord[header.arcount]
}

format DNS_Header {
    id: uint16
    flags
    qdcount: uint16
    ancount: uint16
    nscount: uint16
    arcount: uint16
}

format DNS_Question {
    qname: DomainName
    qtype: uint16
    qclass: uint16
}

format ResourceRecord {
    name: DomainName
    type: uint16
    class: uint16
    ttl: uint32
    rdlength: uint16
    rdata: uint8[rdlength]
}

format DomainName {
    while(true) {
        length: uint8
        if (length == 0) break
        if ((length & 0xC0) == 0xC0) {
            offset: uint8
            break
        }
        label: uint8[length]
    }
}

format SOA_RDATA {
    mname: DomainName
    rname: DomainName
    serial: uint32
    refresh: uint32
    retry: uint32
    expire: uint32
    minimum: uint32
}

format MX_RDATA {
    preference: uint16
    exchange: DomainName
}

format A_RDATA {
    address: uint8[4]
}

format AAAA_RDATA {
    address: uint8[16]
}

format NS_RDATA {
    nsdname: DomainName
}

format CNAME_RDATA {
    cname: DomainName
}

format PTR_RDATA {
    ptrdname: DomainName
}

format TXT_RDATA {
    while(!end) {
        length: uint8
        string: uint8[length]
    }
}