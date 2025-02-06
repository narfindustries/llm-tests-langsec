type DNSHeader = record {
    id: uint16;                       // Identifier

    // Flags
    flags: uint16 {
        qr: uint1;                    // Query/Response Flag
        opcode: uint4;                // Operation Code
        aa: uint1;                    // Authoritative Answer Flag
        tc: uint1;                    // Truncation Flag
        rd: uint1;                    // Recursion Desired
        ra: uint1;                    // Recursion Available
        z: uint3;                     // Reserved for future use
        rcode: uint4;                 // Response Code
    };

    qdcount: uint16;                  // Number of questions
    ancount: uint16;                  // Number of answers
    nscount: uint16;                  // Number of authority records
    arcount: uint16;                  // Number of additional records
};

type DNSQuestion = record {
    qname: DNSName;                   // Domain name
    qtype: uint16;                    // Type of the query
    qclass: uint16;                   // Class of the query
};

type DNSResourceRecord = record {
    name: DNSName;                    // Domain name
    type: uint16;                     // Type
    class: uint16;                    // Class
    ttl: uint32;                      // Time to live
    rdlength: uint16;                 // Length of RDATA
    rdata: bytes[rdlength];           // Resource data
};

type DNSMessage = record {
    header: DNSHeader;                // DNS header
    questions: array of DNSQuestion[header.qdcount];    // Questions
    answers: array of DNSResourceRecord[header.ancount];  // Answer RRs
    authorities: array of DNSResourceRecord[header.nscount];  // Authority RRs
    additionals: array of DNSResourceRecord[header.arcount];  // Additional RRs
};

type DNSName = bytes;               // Sequence of labels (length byte followed by label) ending with zero byte.