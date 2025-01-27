meta:
  id: dns
  name: DNS
  file-extension: null
  endian: le
  license: MIT
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
    type: query
types:
  query:
    seq:
      - id: name
        type: domain-name
      - id: type
        type: u2
      - id: class
        type: u2
  domain-name:
    seq:
      - id: len
        type: u1
      - id: domain
        type: str
        size: len
        encoding: ascii