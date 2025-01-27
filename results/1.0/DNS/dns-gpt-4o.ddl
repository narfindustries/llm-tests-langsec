namespace DNS

-- DNS Header format according to RFC 1035 section 4.1.1
struct DnsHeader {
    id: u16be;
    flags: u16be;
    qd_count: u16be;
    an_count: u16be;
    ns_count: u16be;
    ar_count: u16be;
}

-- Define DNS question section
struct DnsQuestion {
    qname: DnsName;
    qtype: u16be;
    qclass: u16be;
}

-- Define DNS resource record
struct DnsRr {
    name: DnsName;
    rtype: u16be;
    rclass: u16be;
    ttl: u32be;
    rdlength: u16be;
    rdata: bytes(rdlength);
}

-- Recursive structure for domain names
struct DnsName {
    labels: list(DnsLabel);
} : google.protobuf : {
    terminator: DnsLabel { length: 0 }
}

struct DnsLabel {
    length: u8;
    label: bytes(length);
}

-- Define DNS message
struct DnsMessage {
    header: DnsHeader;
    questions: array(header.qd_count) of DnsQuestion;
    answers: array(header.an_count) of DnsRr;
    authorities: array(header.ns_count) of DnsRr;
    additionals: array(header.ar_count) of DnsRr;
}
