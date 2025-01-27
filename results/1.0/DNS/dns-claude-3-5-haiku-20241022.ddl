module DNS

# Basic DNS packet structure
type DNSPacket = {
    header: DNSHeader,
    questions: [DNSQuestion],
    answers: [DNSResourceRecord],
    authority: [DNSResourceRecord],
    additional: [DNSResourceRecord]
}

# DNS Header Structure
type DNSHeader = {
    id: uint16,
    qr: bool,  # Query/Response Flag
    opcode: uint4,
    aa: bool,  # Authoritative Answer 
    tc: bool,  # Truncation Flag
    rd: bool,  # Recursion Desired
    ra: bool,  # Recursion Available
    z: uint3,  # Reserved bits
    rcode: uint4,  # Response Code
    qdcount: uint16,  # Question Count
    ancount: uint16,  # Answer Count
    nscount: uint16,  # Authority Record Count  
    arcount: uint16   # Additional Record Count
}

# DNS Question Structure
type DNSQuestion = {
    qname: [uint8],  # Domain name as sequence of labels
    qtype: uint16,   # Query Type 
    qclass: uint16   # Query Class
}

# DNS Resource Record Structure
type DNSResourceRecord = {
    name: [uint8],   # Domain name 
    type: uint16,    # Resource Record Type
    class: uint16,   # Resource Record Class
    ttl: uint32,     # Time to Live
    rdlength: uint16,# Resource Data Length
    rdata: [uint8]   # Resource Data
}

# Validation rules and constants
const MAX_LABEL_LENGTH = 63
const MAX_DOMAIN_LENGTH = 255
const MAX_PACKET_SIZE = 512

# DNS Query Type Constants
const TYPE_A = 1      # IPv4 Address
const TYPE_AAAA = 28  # IPv6 Address
const TYPE_CNAME = 5  # Canonical Name
const TYPE_MX = 15    # Mail Exchange

# DNS Class Constants
const CLASS_IN = 1    # Internet

# Parsing and validation constraints
constraint DNSPacket {
    # Header constraints
    header.qdcount == len(questions),
    header.ancount == len(answers),
    header.nscount == len(authority),
    header.arcount == len(additional),

    # Packet size constraint
    len(to_bytes(self)) <= MAX_PACKET_SIZE
}

# Domain name label validation
constraint DNSQuestion {
    # Validate domain name labels
    forall label in qname {
        len(label) <= MAX_LABEL_LENGTH
    },
    len(qname) <= MAX_DOMAIN_LENGTH
}