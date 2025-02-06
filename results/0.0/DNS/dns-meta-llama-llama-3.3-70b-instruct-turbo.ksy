meta:
  id: dns
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
        type: flags
      - id: qdcount
        type: u2
      - id: ancount
        type: u2
      - id: nscount
        type: u2
      - id: arcount
        type: u2

  flags:
    bits: 16
    seq:
      - id: qr
        type: u1
      - id: opcode
        type: u4
      - id: aa
        type: u1
      - id: tc
        type: u1
      - id: rd
        type: u1
      - id: ra
        type: u1
      - id: z
        type: u3
      - id: rcode
        type: u4

  question:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2
      - id: class
        type: u2

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
        size: rdlength

  domain_name:
    seq:
      - id: labels
        type: label
        repeat: until
        until: expr
        until-expr: labels[-1].length == 0
      - id: root_label
        type: u1
        value: 0

  label:
    seq:
      - id: length
        type: u1
      - id: label
        type: str
        size: length
        encoding: ascii

  rdata:
    seq:
      - id: type
        type: u2
      - id: data
        type:
          switch-on: type
          cases:
            1: u4
            2: domain_name
            5: domain_name
            6:
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
            12: domain_name
            15:
              seq:
                - id: preference
                  type: u2
                - id: exchange
                  type: domain_name
            16:
              str:
                size: rdlength
                encoding: ascii
            28: u8
            _: 
              str:
                size: rdlength
                encoding: ascii