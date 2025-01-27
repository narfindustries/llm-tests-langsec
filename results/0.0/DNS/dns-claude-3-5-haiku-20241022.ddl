type Byte = [0..255]
type UInt16 = [0..65535]
type UInt32 = [0..4294967295]

type DNSHeader = {
    id: UInt16,
    qr: [0..1],
    opcode: [0..15],
    aa: [0..1],
    tc: [0..1],
    rd: [0..1],
    ra: [0..1],
    z: [0..7],
    rcode: [0..15],
    qdcount: UInt16,
    ancount: UInt16,
    nscount: UInt16,
    arcount: UInt16
}

type DNSQuestion = {
    qname: [Byte],
    qtype: UInt16,
    qclass: UInt16
}

type DNSResourceRecord = {
    name: [Byte],
    type: UInt16,
    class: UInt16,
    ttl: UInt32,
    rdlength: UInt16,
    rdata: [Byte]
}

type DNSMessage = {
    header: DNSHeader,
    questions: [DNSQuestion],
    answers: [DNSResourceRecord],
    authority: [DNSResourceRecord],
    additional: [DNSResourceRecord]
}

let parse_dns_message = (input: [Byte]) -> DNSMessage = {
    header: parse_dns_header(input[0..12]),
    questions: parse_dns_questions(input[12..], header.qdcount),
    answers: parse_dns_resource_records(input[12 + question_length..], header.ancount),
    authority: parse_dns_resource_records(input[12 + question_length + answer_length..], header.nscount),
    additional: parse_dns_resource_records(input[12 + question_length + answer_length + authority_length..], header.arcount)
}

let parse_dns_header = (input: [Byte]) -> DNSHeader = {
    id: (input[0] << 8) | input[1],
    qr: (input[2] >> 7) & 1,
    opcode: (input[2] >> 3) & 15,
    aa: (input[2] >> 2) & 1,
    tc: (input[2] >> 1) & 1,
    rd: input[2] & 1,
    ra: (input[3] >> 7) & 1,
    z: (input[3] >> 4) & 7,
    rcode: input[3] & 15,
    qdcount: (input[4] << 8) | input[5],
    ancount: (input[6] << 8) | input[7],
    nscount: (input[8] << 8) | input[9],
    arcount: (input[10] << 8) | input[11]
}

let parse_dns_questions = (input: [Byte], count: UInt16) -> [DNSQuestion] = {
    let questions = [];
    let offset = 0;
    for i in [0..count) {
        let (question, new_offset) = parse_dns_question(input[offset..]);
        questions = questions ++ [question];
        offset = new_offset;
    }
    questions
}

let parse_dns_question = (input: [Byte]) -> (DNSQuestion, UInt16) = {
    let (qname, name_length) = parse_dns_name(input);
    let qtype = (input[name_length] << 8) | input[name_length + 1];
    let qclass = (input[name_length + 2] << 8) | input[name_length + 3];
    ({
        qname: qname,
        qtype: qtype,
        qclass: qclass
    }, name_length + 4)
}

let parse_dns_name = (input: [Byte]) -> ([Byte], UInt16) = {
    let name = [];
    let offset = 0;
    while input[offset] != 0 {
        let length = input[offset];
        name = name ++ input[offset + 1..offset + 1 + length];
        offset = offset + length + 1;
    }
    (name, offset + 1)
}

let parse_dns_resource_records = (input: [Byte], count: UInt16) -> [DNSResourceRecord] = {
    let records = [];
    let offset = 0;
    for i in [0..count) {
        let (record, new_offset) = parse_dns_resource_record(input[offset..]);
        records = records ++ [record];
        offset = new_offset;
    }
    records
}

let parse_dns_resource_record = (input: [Byte]) -> (DNSResourceRecord, UInt16) = {
    let (name, name_length) = parse_dns_name(input);
    let type = (input[name_length] << 8) | input[name_length + 1];
    let class = (input[name_length + 2] << 8) | input[name_length + 3];
    let ttl = (input[name_length + 4] << 24) | (input[name_length + 5] << 16) | (input[name_length + 6] << 8) | input[name_length + 7];
    let rdlength = (input[name_length + 8] << 8) | input[name_length + 9];
    let rdata = input[name_length + 10..name_length + 10 + rdlength];
    ({
        name: name,
        type: type,
        class: class,
        ttl: ttl,
        rdlength: rdlength,
        rdata: rdata
    }, name_length + 10 + rdlength)
}