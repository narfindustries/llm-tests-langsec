meta:
  id: dns
  name: DNS
  fields:
    - name: transaction_id
      type: u2
    - name: flags
      type: u2
    - name: qdcount
      type: u2
    - name: ancount
      type: u2
    - name: nscount
      type: u2
    - name: arcount
      type: u2
  seq:
    - id: transaction_id
    - id: flags
    - id: qdcount
    - id: ancount
    - id: nscount
    - id: arcount
    - id: questions
      type: seq
      repeat: expr
      repeat-expr: qdcount
      seq:
        - id: name
          type: str
          terminator: 0
        - id: type
          type: u2
        - id: class
          type: u2
    - id: answers
      type: seq
      repeat: expr
      repeat-expr: ancount
      seq:
        - id: name
          type: u2
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
          size: rdlength