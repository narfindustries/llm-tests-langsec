types:
  domain_name:
    seq:
      - id: length
        type: u2
      - id: label_len
        type: u1
        read: label_len_read
      - id: label
        type: str
        len: label_len
        terminator: 0
        read: label_read
      - id: rest
        type: domain_name
        if: label_len > 0

  resource_record:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2
      - id: class
        type: u2
      - id: ttl
        type: u4
      - id: rdlength
        type: u2
      - id: rdata
        type: rdata

  rdata:
    switch_on: type
    cases:
      1:
        id: a_record
        seq:
          - id: address
            type: ip4
      28:
        id: aaaa_record
        seq:
          - id: address
            type: ip6
      5:
        id: cname_record
        seq:
          - id: cname
            type: domain_name
      15:
        id: mx_record
        seq:
          - id: preference
            type: u2
          - id: exchange
            type: domain_name
      6:
        id: soa_record
        seq:
          - id: mname
            type: domain_name
          - id: rname
            type: domain_name
          - id: serial
            type: u4
          - id: refresh
            type: u4
          - id: retry
            type: u4
          - id: expire
            type: u4
          - id: minimum
            type: u4
      12:
        id: ptr_record
        seq:
          - id: ptr
            type: domain_name
      16:
        id: txt_record
        seq:
          - id: txt_data
            type: strz

  dns_header:
    seq:
      - id: id
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

  dns_question:
    seq:
      - id: qname
        type: domain_name
      - id: qtype
        type: u2
      - id: qclass
        type: u2

root:
  seq:
    - id: header
      type: dns_header
    - id: questions
      type: dns_question
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
