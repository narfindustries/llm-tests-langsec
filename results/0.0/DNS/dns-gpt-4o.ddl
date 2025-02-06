PDU DNSMessage {
    uint16 id;
    BitField16 flags {
        uint1 qr;
        uint4 opcode;
        uint1 aa;
        uint1 tc;
        uint1 rd;
        uint1 ra;
        uint3 z;
        uint4 rcode;
    }
    uint16 qdcount;
    uint16 ancount;
    uint16 nscount;
    uint16 arcount;

    for (int i = 0; i < qdcount; i++) {
        Question questions;
    }

    for (int i = 0; i < ancount; i++) {
        ResourceRecord answers;
    }

    for (int i = 0; i < nscount; i++) {
        ResourceRecord authorities;
    }

    for (int i = 0; i < arcount; i++) {
        ResourceRecord additionals;
    }
}

PDU Question {
    DomainName qname;
    uint16 qtype;
    uint16 qclass;
}

PDU ResourceRecord {
    DomainName name;
    uint16 type;
    uint16 class;
    uint32 ttl;
    uint16 rdlength;
    RData rdata;
}

PDU RData {
    switch (outer.type) {
        case 1: IPv4Address;
        case 2: DomainName;
        case 5: DomainName;
        case 6: SOARecord;
        case 12: DomainName;
        case 15: MXRecord;
        case 28: IPv6Address;
        default: RawData;
    }
}

PDU IPv4Address {
    uint8 octet[4];
}

PDU IPv6Address {
    uint16 segment[8];
}

PDU SOARecord {
    DomainName mname;
    DomainName rname;
    uint32 serial;
    uint32 refresh;
    uint32 retry;
    uint32 expire;
    uint32 minimum;
}

PDU MXRecord {
    uint16 preference;
    DomainName exchange;
}

PDU DomainName {
    while (true) {
        uint8 length;
        if (length == 0) break;
        uint8 label[length];
    }
}

PDU RawData {
    uint8 data[outer.rdlength];
}