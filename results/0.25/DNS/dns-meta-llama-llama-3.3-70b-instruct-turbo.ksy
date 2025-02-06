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
        type: uint16
      - id: class
        type: uint16

  domain_name:
    seq:
      - id: labels
        type: label
        repeat: until
        until: label.length == 0
    types:
      label:
        seq:
          - id: length
            type: u1
          - id: label
            type: str
            size: length

  resource_record:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: uint16
      - id: class
        type: uint16
      - id: ttl
        type: uint32
      - id: rdlength
        type: uint16
      - id: rdata
        type: rdata
        size: rdlength

  rdata:
    seq:
      - id: data
        type:
          switch: type
          cases:
            1: # A
              type: a
            2: # NS
              type: ns
            5: # CNAME
              type: cname
            6: # SOA
              type: soa
            12: # PTR
              type: ptr
            15: # MX
              type: mx
            28: # AAAA
              type: aaaa
            252: # AXFR
              type: axfr
            253: # MAILB
              type: mailb
            254: # MAILA
              type: maila
            255: # *
              type: wildcard
        types:
          a:
            seq:
              - id: address
                type: uint32
                repeat: 4
          ns:
            seq:
              - id: nsdname
                type: domain_name
          cname:
            seq:
              - id: cname
                type: domain_name
          soa:
            seq:
              - id: mname
                type: domain_name
              - id: rname
                type: domain_name
              - id: serial
                type: uint32
              - id: refresh
                type: uint32
              - id: retry
                type: uint32
              - id: expire
                type: uint32
              - id: minimum
                type: uint32
          ptr:
            seq:
              - id: ptrdname
                type: domain_name
          mx:
            seq:
              - id: preference
                type: uint16
              - id: exchange
                type: domain_name
          aaaa:
            seq:
              - id: address
                type: uint32
                repeat: 8
          axfr:
            seq:
              - id: axfr
                type: str
                size: rdlength
          mailb:
            seq:
              - id: mbdomain
                type: domain_name
          maila:
            seq:
              - id: madomain
                type: domain_name
          wildcard:
            seq:
              - id: wildcard
                type: str
                size: rdlength