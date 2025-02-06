protocol DNS {
    message {
        header {
            uint16 id;
            bitfield16 flags {
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
        }

        question_section [qdcount] {
            question {
                domain_name qname;
                uint16 qtype;
                uint16 qclass;
            }
        }

        answer_section [ancount] {
            resource_record {
                domain_name name;
                uint16 type;
                uint16 class;
                uint32 ttl;
                uint16 rdlength;
                rdata rdata;
            }
        }

        authority_section [nscount] {
            resource_record {
                domain_name name;
                uint16 type;
                uint16 class;
                uint32 ttl;
                uint16 rdlength;
                rdata rdata;
            }
        }

        additional_section [arcount] {
            resource_record {
                domain_name name;
                uint16 type;
                uint16 class;
                uint32 ttl;
                uint16 rdlength;
                rdata rdata;
            }
        }
    }

    domain_name {
        length_prefixed_string {
            uint8 length;
            char[length] label;
        } [until: length == 0]
    }

    rdata {
        switch (type) {
            case 1: ipv4_address;
            case 28: ipv6_address;
            case 2: domain_name; // NS
            case 5: domain_name; // CNAME
            case 12: domain_name; // PTR
            case 6: soa_record;
            case 15: mx_record;
            case 16: txt_record;
            case 33: srv_record;
            default: raw_bytes;
        }
    }

    ipv4_address {
        uint8 octet[4];
    }

    ipv6_address {
        uint16 segment[8];
    }

    soa_record {
        domain_name mname;
        domain_name rname;
        uint32 serial;
        uint32 refresh;
        uint32 retry;
        uint32 expire;
        uint32 minimum;
    }

    mx_record {
        uint16 preference;
        domain_name exchange;
    }

    txt_record {
        length_prefixed_string {
            uint8 length;
            char[length] text;
        } [until: length == 0]
    }

    srv_record {
        uint16 priority;
        uint16 weight;
        uint16 port;
        domain_name target;
    }

    raw_bytes {
        uint8 data[rdlength];
    }
}