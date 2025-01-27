type byte = uint8
type bytes = [byte]

type DNSHeader = {
    id: uint16,
    qr: bool,
    opcode: uint4,
    aa: bool,
    tc: bool,
    rd: bool,
    ra: bool,
    z: uint3,
    rcode: uint4
}

type DNSQuestion = {
    qname: [string],
    qtype: uint16,
    qclass: uint16
}

type DNSResourceRecord = {
    name: [string],
    type: uint16,
    class: uint16,
    ttl: uint32,
    rdlength: uint16,
    rdata: bytes
}

type DNSMessage = {
    header: DNSHeader,
    questions: [DNSQuestion],
    answers: [DNSResourceRecord],
    authority: [DNSResourceRecord], 
    additional: [DNSResourceRecord]
}

let parse_dns_message = fun (input: bytes) -> DNSMessage:
    let (header, rest1) = parse_header input in
    let (questions, rest2) = parse_questions rest1 header.qdcount in
    let (answers, rest3) = parse_resource_records rest2 header.ancount in
    let (authority, rest4) = parse_resource_records rest3 header.nscount in
    let (additional, _) = parse_resource_records rest4 header.arcount in
    {
        header = header,
        questions = questions,
        answers = answers, 
        authority = authority,
        additional = additional
    }

let parse_header = fun (input: bytes) -> (DNSHeader, bytes):
    let id = parse_uint16 input in
    let flags = parse_uint16 (drop input 2) in
    {
        id = id,
        qr = (flags >> 15) & 1 == 1,
        opcode = (flags >> 11) & 0xF,
        aa = (flags >> 10) & 1 == 1,
        tc = (flags >> 9) & 1 == 1,
        rd = (flags >> 8) & 1 == 1,
        ra = (flags >> 7) & 1 == 1,
        z = (flags >> 4) & 0x7,
        rcode = flags & 0xF
    }

let parse_questions = fun (input: bytes) (count: uint16) -> ([DNSQuestion], bytes):
    let rec parse_question_list = fun (input: bytes) (count: uint16) (acc: [DNSQuestion]) ->
        if count == 0 then (reverse acc, input)
        else 
            let (qname, rest1) = parse_qname input in
            let qtype = parse_uint16 rest1 in
            let qclass = parse_uint16 (drop rest1 2) in
            let question = { qname = qname, qtype = qtype, qclass = qclass } in
            parse_question_list (drop rest1 4) (count - 1) (question :: acc)
    in
    parse_question_list input count []

let parse_resource_records = fun (input: bytes) (count: uint16) -> ([DNSResourceRecord], bytes):
    let rec parse_record_list = fun (input: bytes) (count: uint16) (acc: [DNSResourceRecord]) ->
        if count == 0 then (reverse acc, input)
        else
            let (name, rest1) = parse_qname input in
            let type = parse_uint16 rest1 in
            let class = parse_uint16 (drop rest1 2) in
            let ttl = parse_uint32 (drop rest1 4) in
            let rdlength = parse_uint16 (drop rest1 8) in
            let rdata = take (drop rest1 10) rdlength in
            let record = { 
                name = name, 
                type = type, 
                class = class, 
                ttl = ttl, 
                rdlength = rdlength, 
                rdata = rdata 
            } in
            parse_record_list (drop rest1 (10 + rdlength)) (count - 1) (record :: acc)
    in
    parse_record_list input count []

let parse_qname = fun (input: bytes) -> ([string], bytes):
    let rec parse_labels = fun (input: bytes) (acc: [string]) ->
        let length = input[0] in
        if length == 0 then (reverse acc, drop input 1)
        else 
            let label = take (drop input 1) length in
            parse_labels (drop input (length + 1)) (label :: acc)
    in
    parse_labels input []