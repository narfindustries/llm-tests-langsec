def DNS = struct {
    header struct {
        id uint16
        flags struct {
            qr uint1
            opcode uint4
            aa uint1
            tc uint1
            rd uint1
            ra uint1
            z uint3
            rcode uint4
        }
        qdcount uint16
        ancount uint16
        nscount uint16
        arcount uint16
    }

    DomainName struct {
        length uint8
        switch length {
            case 0: null
            case ($$ & 0xc0) == 0xc0: pointer uint8
            default: {
                label bytes(length)
                next DomainName
            }
        }
    }

    Question struct {
        qname DomainName
        qtype uint16
        qclass uint16
    }

    ResourceRecord struct {
        name DomainName
        type uint16
        class uint16
        ttl uint32
        rdlength uint16
        rdata bytes(rdlength)
    }

    questions Question[header.qdcount]
    answers ResourceRecord[header.ancount]
    authority ResourceRecord[header.nscount]
    additional ResourceRecord[header.arcount]
}