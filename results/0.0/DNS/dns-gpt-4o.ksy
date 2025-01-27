meta:
  id: dns_gpt_4o
  title: DNS Packet
  file-extension: dns
  endian: be

seq:
  - id: transaction_id
    type: u2

  - id: flags
    type: u2
    doc: |
      A 16-bit field containing various flags.

  - id: qdcount
    type: u2
    doc: |
      Number of entries in the question section.

  - id: ancount
    type: u2
    doc: |
      Number of resource records in the answer section.

  - id: nscount
    type: u2
    doc: |
      Number of name server resource records in the authority records section.

  - id: arcount
    type: u2
    doc: |
      Number of resource records in the additional records section.

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
  question:
    seq:
      - id: qname
        type: strz
        encoding: ascii
        terminator: 0
        doc: |
          Domain name being queried.

      - id: qtype
        type: u2
        doc: |
          Type of the query.

      - id: qclass
        type: u2
        doc: |
          Class of the query.

  resource_record:
    seq:
      - id: name
        type: strz
        encoding: ascii
        terminator: 0
        doc: |
          Domain name of the resource record.

      - id: type
        type: u2
        doc: |
          Type of the resource record.

      - id: class
        type: u2
        doc: |
          Class of the resource record.

      - id: ttl
        type: u4
        doc: |
          Time to live for the resource record.

      - id: rdlength
        type: u2
        doc: |
          Length of the RDATA field.

      - id: rdata
        size: rdlength
        doc: |
          Data of the resource record.