meta:
  id: dns_packet
  title: DNS Packet
  endian: be

seq:
  - id: header
    type: header

  - id: questions
    type: question
    repeat: expr
    repeat-expr: header.qdcount

  - id: answers
    type: resource_record
    repeat: expr
    repeat-expr: header.ancount

  - id: authorities
    type: resource_record
    repeat: expr
    repeat-expr: header.nscount

  - id: additionals
    type: resource_record
    repeat: expr
    repeat-expr: header.arcount

types:
  header:
    seq:
      - id: id
        type: u2

      - id: flags
        type: u2
        doc: |
          QR (1 bit), Opcode (4 bits), AA (1 bit), TC (1 bit), RD (1 bit),
          RA (1 bit), Z (3 bits), RCODE (4 bits)

      - id: qdcount
        type: u2
        doc: Number of entries in the question section

      - id: ancount
        type: u2
        doc: Number of resource records in the answer section

      - id: nscount
        type: u2
        doc: Number of name server resource records in the authority records section

      - id: arcount
        type: u2
        doc: Number of resource records in the additional records section

  question:
    seq:
      - id: qname
        type: domain_name

      - id: qtype
        type: u2
        doc: |
          A two-octet code specifying the type of the query. Common values include:
          1 (A), 2 (NS), 5 (CNAME), 6 (SOA), 12 (PTR), 15 (MX), 28 (AAAA), 255 (*)

      - id: qclass
        type: u2
        doc: |
          A two-octet code that specifies the class of the query. Common values include:
          1 (IN), 3 (CH), 4 (HS), 255 (*)

  resource_record:
    seq:
      - id: name
        type: domain_name

      - id: type
        type: u2
        doc: Same as QTYPE in the question section

      - id: class
        type: u2
        doc: Same as QCLASS in the question section

      - id: ttl
        type: u4
        doc: Time interval (in seconds) that the resource record may be cached

      - id: rdlength
        type: u2
        doc: Length in octets of the RDATA field

      - id: rdata
        size: rdlength
        doc: Variable-length string of octets that describes the resource

  domain_name:
    seq:
      - id: name
        type: strz
        encoding: ascii
        terminator: 0
        repeat: eos
        doc: Domain name represented as a sequence of labels