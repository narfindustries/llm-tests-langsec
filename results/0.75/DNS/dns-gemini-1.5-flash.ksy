types:
  domain_name:
    seq:
      - id: length
        type: u2be
      - id: data
        type: str
        size: length
  resource_record:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2be
      - id: class
        type: u2be
      - id: ttl
        type: u4be
      - id: rdlength
        type: u2be
      - id: rdata
        type: bytes
        size: rdlength
  dns_header:
    seq:
      - id: id
        type: u2be
      - id: flags
        type: u2be
      - id: qdcount
        type: u2be
      - id: ancount
        type: u2be
      - id: nscount
        type: u2be
      - id: arcount
        type: u2be
  dns_message:
    seq:
      - id: header
        type: dns_header
      - id: questions
        type: seq
        size: header.qdcount
        read: question
      - id: answers
        type: seq
        size: header.ancount
        read: resource_record
      - id: authorities
        type: seq
        size: header.nscount
        read: resource_record
      - id: additionals
        type: seq
        size: header.arcount
        read: resource_record
  question:
    seq:
      - id: qname
        type: domain_name
      - id: qtype
        type: u2be
      - id: qclass
        type: u2be

