def DNS = {
    header: DNSHeader
    questions: DNSQuestion[]
    answers: ResourceRecord[]
    authorities: ResourceRecord[]
    additionals: ResourceRecord[]
}

def DNSHeader = {
    id: uint16
    flags: DNSFlags
    qdcount: uint16
    ancount: uint16
    nscount: uint16
    arcount: uint16
}

def DNSFlags = {
    qr: uint1
    opcode: uint4
    aa: uint1
    tc: uint1
    rd: uint1
    ra: uint1
    z: uint3
    rcode: uint4
}

def DNSQuestion = {
    qname: DomainName
    qtype: uint16
    qclass: uint16
}

def ResourceRecord = {
    name: DomainName
    type: uint16
    class: uint16
    ttl: uint32
    rdlength: uint16
    rdata: bytes(rdlength)
}

def DomainName = {
    labels: Label[]
    terminator: uint8 where terminator == 0
}

def Label = {
    length: uint8 where length > 0 and length <= 63
    name: bytes(length)
}

def Pointer = {
    pointer: uint16 where (pointer & 0xC000) == 0xC000
}

def SOA_RDATA = {
    mname: DomainName
    rname: DomainName
    serial: uint32
    refresh: uint32
    retry: uint32
    expire: uint32
    minimum: uint32
}

def MX_RDATA = {
    preference: uint16
    exchange: DomainName
}

def A_RDATA = {
    address: bytes(4)
}

def AAAA_RDATA = {
    address: bytes(16)
}

def TXT_RDATA = {
    txt_length: uint8
    txt_data: bytes(txt_length)
}

def NS_RDATA = {
    nsdname: DomainName
}

def CNAME_RDATA = {
    cname: DomainName
}

def PTR_RDATA = {
    ptrdname: DomainName
}