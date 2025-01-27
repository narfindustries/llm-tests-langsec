meta:
  id: icmp-gemini-1
  title: ICMP Gemini 1.5 Flash
  endian: be

seq:
  - id: header
    type: header_t
  - id: data
    type: seq
    repeat: expr
    repeat-expr: header.length - 8
    contents:
      - id: byte
        type: u1

types:
  header_t:
    seq:
      - id: type
        type: u1
      - id: code
        type: u1
      - id: checksum
        type: u2
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
      - id: length
        type: u2

