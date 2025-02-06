types:
  icmp_header:
    seq:
      - id: type
        type: u1
      - id: code
        type: u1
      - id: checksum
        type: u2le
      - id: identifier
        type: u2le
      - id: sequence_number
        type: u2le
    instances:
      - id: data
        type: seq
        size: (this.size - 8)
        contents:
          - type: u1
