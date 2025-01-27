meta:
  id: dns_packet
  title: DNS Packet
  application: DNS
  license: CC0-1.0
  endian: be

seq:
  - id: transaction_id
    type: u2

  - id: flags
    type: flags

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

  - id: questions
    type: question
    repeat: expr
    repeat-expr: qdcount

  - id: answers
    type: resource_record
    repeat: expr
    repeat-expr: ancount

  - id: authorities
    type: resource_record
    repeat: expr
    repeat-expr: nscount

  - id: additionals
    type: resource_record
    repeat: expr
    repeat-expr: arcount

types:
  flags:
    seq:
      - id: qr
        type: b1
        doc: Query/Response Flag

      - id: opcode
        type: b4
        doc: Operation Code

      - id: aa
        type: b1
        doc: Authoritative Answer Flag

      - id: tc
        type: b1
        doc: Truncation Flag

      - id: rd
        type: b1
        doc: Recursion Desired

      - id: ra
        type: b1
        doc: Recursion Available

      - id: z
        type: b3
        doc: Reserved for future use, must be zero

      - id: rcode
        type: b4
        doc: Response Code

  question:
    seq:
      - id: qname
        type: domain_name
        doc: Domain name

      - id: qtype
        type: u2
        doc: Type of the query

      - id: qclass
        type: u2
        doc: Class of the query

  resource_record:
    seq:
      - id: name
        type: domain_name
        doc: Domain name

      - id: type
        type: u2
        doc: Type of the resource record

      - id: class
        type: u2
        doc: Class of the resource record

      - id: ttl
        type: u4
        doc: Time to live

      - id: rdlength
        type: u2
        doc: Length of the RDATA field

      - id: rdata
        size: rdlength
        doc: Resource data

  domain_name:
    doc: |
      Domain names are a series of labels, where each label consists of a length
      octet followed by that number of octets. The domain name terminates with
      the zero length octet for the null label of the root.
    seq:
      - id: name
        type: strz
        encoding: ascii
        terminator: 0
        repeat: eos
        doc: Domain name labels