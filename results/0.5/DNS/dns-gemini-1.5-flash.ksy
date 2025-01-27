types:
  - id: dns_header
    fields:
      - id: transaction_id
        type: u2
      - id: flags
        type: u2
      - id: questions
        type: u2
      - id: answers
        type: u2
      - id: authority
        type: u2
      - id: additional
        type: u2

  - id: dns_question
    fields:
      - id: qname
        type: str
        encoding: UTF-8
      - id: qtype
        type: u2
      - id: qclass
        type: u2

  - id: dns_resource_record
    fields:
      - id: name
        type: str
        encoding: UTF-8
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


  - id: dns_packet
    fields:
      - id: header
        type: dns_header
      - id: questions
        type: seq
        size: header.questions
        type: dns_question
      - id: answers
        type: seq
        size: header.answers
        type: dns_resource_record
      - id: authority
        type: seq
        size: header.authority
        type: dns_resource_record
      - id: additional
        type: seq
        size: header.additional
        type: dns_resource_record

