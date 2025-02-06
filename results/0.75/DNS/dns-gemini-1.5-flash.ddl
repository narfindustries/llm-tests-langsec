structure dns_header {
    uint16 transaction_id;
    uint16 flags;
    uint16 questions;
    uint16 answers;
    uint16 nscount;
    uint16 arcount;
}

structure domain_name {
    array label: uint8;
}

structure dns_question {
    domain_name qname;
    uint16 qtype;
    uint16 qclass;
}

structure dns_rdata {
    uint16 type;
    bytes data;
}


structure dns_resource_record {
    domain_name name;
    uint16 type;
    uint16 class;
    uint32 ttl;
    uint16 rdlength;
    bytes rdata;
}

type dns_message = dns_header, array[dns_question], array[dns_resource_record], array[dns_resource_record], array[dns_resource_record];
