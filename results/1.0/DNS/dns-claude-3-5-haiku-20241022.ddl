type DnsMessage = record {
    header: DnsHeader,
    questions: array(header.questionCount) of DnsQuestion,
    answers: array(header.answerCount) of DnsResourceRecord,
    authorities: array(header.authorityCount) of DnsResourceRecord,
    additional: array(header.additionalCount) of DnsResourceRecord
}

type DnsHeader = record {
    transactionId: uint16,
    flags: record {
        qr: bit,
        opcode: uint4 where value <= 2,
        aa: bit,
        tc: bit, 
        rd: bit,
        ra: bit,
        z: uint3,
        rcode: uint4 where value <= 5
    },
    questionCount: uint16,
    answerCount: uint16,
    authorityCount: uint16,
    additionalCount: uint16
}

type DnsQuestion = record {
    qname: DnsDomainName,
    qtype: uint16,
    qclass: uint16
}

type DnsResourceRecord = record {
    name: DnsDomainName,
    type: uint16,
    class: uint16,
    ttl: int32,
    rdLength: uint16,
    rdata: array(rdLength) of uint8
}

type DnsDomainName = record {
    labels: list of DnsLabel while !endOfInput,
    terminator: uint8 = 0
}

type DnsLabel = record {
    length: uint8 where value < 64,
    data: array(length) of uint8 if length < 64,
    compressionPointer: uint16 if length >= 192
}

type IPv4Address = array(4) of uint8
type IPv6Address = array(16) of uint8