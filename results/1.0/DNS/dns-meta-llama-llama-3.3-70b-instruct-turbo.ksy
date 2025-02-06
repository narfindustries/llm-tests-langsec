meta:
  id: dns
  endian: be

seq:
  - id: uint16
  - qr: uint1
  - opcode: uint4
  - aa: uint1
  - tc: uint1
  - rd: uint1
  - ra: uint1
  - z: uint3
  - rcode: uint4
  - qdcount: uint16
  - ancount: uint16
  - nscount: uint16
  - arcount: uint16

repeat: expr: qdcount
  - name:
    repeat: until: expr: _ == 0
      - len: uint8
      - data: bytes => len
  - qtype: uint16
  - qclass: uint16

repeat: expr: ancount
  - name:
    repeat: until: expr: _ == 0
      - len: uint8
      - data: bytes => len
  - type: uint16
  - class: uint16
  - ttl: uint32
  - rdlength: uint16
  - rdata:
    type: 
      switch-on: type
      cases:
        1: 
          seq:
            - addr: uint32
        2: 
          seq:
            - nsdname:
              repeat: until: expr: _ == 0
                - len: uint8
                - data: bytes => len
        5: 
          seq:
            - cname:
              repeat: until: expr: _ == 0
                - len: uint8
                - data: bytes => len
        6: 
          seq:
            - mname:
              repeat: until: expr: _ == 0
                - len: uint8
                - data: bytes => len
            - rname:
              repeat: until: expr: _ == 0
                - len: uint8
                - data: bytes => len
            - serial: uint32
            - refresh: uint32
            - retry: uint32
            - expire: uint32
            - minimum: uint32
        else: 
          seq:
            - data: bytes => rdlength

repeat: expr: nscount
  - name:
    repeat: until: expr: _ == 0
      - len: uint8
      - data: bytes => len
  - type: uint16
  - class: uint16
  - ttl: uint32
  - rdlength: uint16
  - rdata:
    type: 
      switch-on: type
      cases:
        1: 
          seq:
            - addr: uint32
        2: 
          seq:
            - nsdname:
              repeat: until: expr: _ == 0
                - len: uint8
                - data: bytes => len
        5: 
          seq:
            - cname:
              repeat: until: expr: _ == 0
                - len: uint8
                - data: bytes => len
        6: 
          seq:
            - mname:
              repeat: until: expr: _ == 0
                - len: uint8
                - data: bytes => len
            - rname:
              repeat: until: expr: _ == 0
                - len: uint8
                - data: bytes => len
            - serial: uint32
            - refresh: uint32
            - retry: uint32
            - expire: uint32
            - minimum: uint32
        else: 
          seq:
            - data: bytes => rdlength

repeat: expr: arcount
  - name:
    repeat: until: expr: _ == 0
      - len: uint8
      - data: bytes => len
  - type: uint16
  - class: uint16
  - ttl: uint32
  - rdlength: uint16
  - rdata:
    type: 
      switch-on: type
      cases:
        1: 
          seq:
            - addr: uint32
        2: 
          seq:
            - nsdname:
              repeat: until: expr: _ == 0
                - len: uint8
                - data: bytes => len
        5: 
          seq:
            - cname:
              repeat: until: expr: _ == 0
                - len: uint8
                - data: bytes => len
        6: 
          seq:
            - mname:
              repeat: until: expr: _ == 0
                - len: uint8
                - data: bytes => len
            - rname:
              repeat: until: expr: _ == 0
                - len: uint8
                - data: bytes => len
            - serial: uint32
            - refresh: uint32
            - retry: uint32
            - expire: uint32
            - minimum: uint32
        else: 
          seq:
            - data: bytes => rdlength