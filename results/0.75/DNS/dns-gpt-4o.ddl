module DNS;

type DNSPacket = struct {
    id                : uint16;
    flags             : DNSFlags;
    question_count    : uint16;
    answer_count      : uint16;
    authority_count   : uint16;
    additional_count  : uint16;
    questions         : Question[question_count];
    answers           : ResourceRecord[answer_count];
    authorities       : ResourceRecord[authority_count];
    additionals       : ResourceRecord[additional_count];
};

type DNSFlags = bitfield {
    qr                 : bool;      // Query/Response Flag
    opcode             : uint4;     // Operation Code
    authoritative      : bool;      // Authoritative Answer
    truncated          : bool;      // Truncated
    recursion_desired  : bool;      // Recursion Desired
    recursion_available: bool;      // Recursion Available
    z                  : uint3;     // Reserved
    rcode              : uint4;     // Response Code
};

type Question = struct {
    qname  : DomainName;
    qtype  : QueryType;
    qclass : QueryClass;
};

type ResourceRecord = struct {
    name     : DomainName;
    type     : QueryType;
    class    : QueryClass;
    ttl      : uint32;
    rdlength : uint16;
    rdata    : bytes[rdlength];
};

type DomainName = listof(DomainLabel, 0x00);

type DomainLabel = prefixed_string<uint8>;

enum QueryType : uint16 {
    A     = 1,
    NS    = 2,
    CNAME = 5,
    SOA   = 6,
    PTR   = 12,
    MX    = 15,
    TXT   = 16,
    AAAA  = 28
};

enum QueryClass : uint16 {
    IN = 1,
    CS = 2,
    CH = 3,
    HS = 4
};