def Main = DNS_Message

def DNS_Message = {
    header: DNS_Header
    questions: DNS_Question[]
    answers: DNS_Resource_Record[]
    authority: DNS_Resource_Record[]
    additional: DNS_Resource_Record[]
}

def DNS_Header = {
    id: uint16
    qr: uint1
    opcode: uint4
    aa: uint1
    tc: uint1
    rd: uint1
    ra: uint1
    z: uint3
    rcode: uint4
    qdcount: uint16
    ancount: uint16
    nscount: uint16
    arcount: uint16
}

def DNS_Question = {
    qname: DNS_Name
    qtype: uint16
    qclass: uint16
}

def DNS_Resource_Record = {
    name: DNS_Name
    type: uint16
    class: uint16
    ttl: uint32
    rdlength: uint16
    rdata: bytes(rdlength)
}

def DNS_Name = {
    var result: bytes = []
    while (true) {
        len: uint8
        if (len == 0) break
        if ((len & 0xC0) == 0xC0) {
            pointer: uint16 = ((len & 0x3F) << 8) | uint8
            break
        }
        label: bytes(len)
        result = result + label + b"."
    }
    result
}

def DNS_RDATA(type: uint16, len: uint16) = {
    switch (type) {
        case 1: # A
            addr: bytes(4)
        case 2: # NS
            nsdname: DNS_Name
        case 5: # CNAME
            cname: DNS_Name
        case 6: # SOA
            mname: DNS_Name
            rname: DNS_Name
            serial: uint32
            refresh: uint32
            retry: uint32
            expire: uint32
            minimum: uint32
        case 12: # PTR
            ptrdname: DNS_Name
        case 15: # MX
            preference: uint16
            exchange: DNS_Name
        case 28: # AAAA
            addr: bytes(16)
        default:
            data: bytes(len)
    }
}

def DNS_Type = {
    A = 1
    NS = 2
    CNAME = 5
    SOA = 6
    PTR = 12
    HINFO = 13
    MX = 15
    AAAA = 28
    AXFR = 252
    ANY = 255
}

def DNS_Class = {
    IN = 1
    CS = 2
    CH = 3
    HS = 4
    ANY = 255
}

def DNS_RCODE = {
    NOERROR = 0
    FORMERR = 1
    SERVFAIL = 2
    NXDOMAIN = 3
    NOTIMP = 4
    REFUSED = 5
}