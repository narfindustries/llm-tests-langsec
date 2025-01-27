meta:
  id: dns_packet
  file-format: dns
seq:
  - id: transaction_id
    type: u2
  - id: flags
    type: u2
  - id: qdcount
    type: u2
  - id: ancount
    type: u2
  - id: nscount
    type: u2
  - id: arcount
    type: u2
  - id: query
    type: dns_query
  - id: answer
    type: dns_answer
    repeat: expr
    repeat-expr: ancount
types:
  dns_query:
    seq:
      - id: name
        type: str
        encoding: utf-8
      - id: type
        type: u2
      - id: class
        type: u2
  dns_answer:
    seq:
      - id: name
        type: str
        encoding: utf-8
      - id: type
        type: u2
      - id: class
        type: u2
      - id: ttl
        type: u4
      - id: rdlength
        type: u2
      - id: rdata
        type: str
        encoding: utf-8
        size: rdlength