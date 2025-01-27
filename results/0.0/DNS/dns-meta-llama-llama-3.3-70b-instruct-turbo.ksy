meta:
  id: dns
  title: DNS
  endian: le

seq:
  - id: transaction_id
    size: 2
  - id: flags
    size: 2
  - id: qdcount
    size: 2
  - id: ancount
    size: 2
  - id: nscount
    size: 2
  - id: arcount
    size: 2
  - id: queries
    type: query
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
  query:
    seq:
      - id: name
        type: domain_name
      - id: type
        size: 2
      - id: class
        size: 2

  domain_name:
    seq:
      - id: labels
        type: label
        repeat: until
        until: ((labels[-1].length == 0) or (labels[-1].length > 63))
      - id: root_label
        size: 1
        if: labels[-1].length == 0

    types:
      label:
        seq:
          - id: length
            size: 1
          - id: label
            size: length
            if: length != 0

  resource_record:
    seq:
      - id: name
        type: domain_name
      - id: type
        size: 2
      - id: class
        size: 2
      - id: ttl
        size: 4
      - id: rdlength
        size: 2
      - id: rdata
        size: rdlength