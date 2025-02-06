type dns_header = {
    transaction_id: uint16,
    flags: {
        qr: bit,
        opcode: uint4,
        aa: bit,
        tc: bit,
        rd: bit,
        ra: bit,
        z: uint3,
        rcode: uint4
    },
    question_count: uint16,
    answer_count: uint16,
    authority_count: uint16,
    additional_count: uint16
};

type dns_label = 
    | ShortLabel(uint8, [uint8])
    | CompressedLabel(uint16);

type dns_name = [dns_label];

type dns_question = {
    name: dns_name,
    type: uint16,
    class: uint16
};

type dns_resource_record = {
    name: dns_name,
    type: uint16,
    class: uint16,
    ttl: uint32,
    rdlength: uint16,
    rdata: [uint8]
};

type dns_message = {
    header: dns_header,
    questions: [dns_question],
    answers: [dns_resource_record],
    authorities: [dns_resource_record],
    additionals: [dns_resource_record]
};