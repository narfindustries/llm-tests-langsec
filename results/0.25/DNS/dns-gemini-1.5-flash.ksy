type: seq
- id: id
  type: u2
- id: flags
  type: u2
  seq:
  - id: qr
    type: bool
    size: 1
  - id: opcode
    type: u4
    size: 4
  - id: aa
    type: bool
    size: 1
  - id: tc
    type: bool
    size: 1
  - id: rd
    type: bool
    size: 1
  - id: ra
    type: bool
    size: 1
  - id: z
    type: u3
    size: 3
  - id: rcode
    type: u4
    size: 4
- id: qdcount
  type: u2
- id: ancount
  type: u2
- id: nscount
  type: u2
- id: arcount
  type: u2
- id: questions
  type: seq
  size: qdcount
  - id: qname
    type: dns_name
  - id: qtype
    type: u2
  - id: qclass
    type: u2
- id: answers
  type: seq
  size: ancount
  - id: record
    type: resource_record
- id: authorities
  type: seq
  size: nscount
  - id: record
    type: resource_record
- id: additionals
  type: seq
  size: arcount
  - id: record
    type: resource_record

types:
  dns_name:
    type: str
    encoding: utf-8
    term: "."
    repeat: true
    instances: true
  resource_record:
    type: seq
    - id: name
      type: dns_name
    - id: type
      type: u2
    - id: class
      type: u2
    - id: ttl
      type: u4
    - id: rdlength
      type: u2
    - id: rdata
      type: bytes
      size: rdlength

