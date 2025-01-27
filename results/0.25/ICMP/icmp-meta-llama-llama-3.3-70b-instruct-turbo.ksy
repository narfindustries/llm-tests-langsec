meta:
  id: icmp
  title: ICMP
  endian: big
seq:
  - id: type
    type: u1
  - id: code
    type: u1
  - id: checksum
    type: u2
  - id: body
    type: switch-on type
    cases:
      0: seq:
          - id: identifier
            type: u2
          - id: sequence_number
            type: u2
      3: seq:
          - id: unused
            type: u2
          - id: ip_header
            type: seq
            seq:
              - id: src_ip_addr
                type: u4
              - id: dst_ip_addr
                type: u4
      5: seq:
          - id: unused
            type: u2
          - id: ip_header
            type: seq
            seq:
              - id: src_ip_addr
                type: u4
              - id: dst_ip_addr
                type: u4
      8: seq:
          - id: identifier
            type: u2
          - id: sequence_number
            type: u2
      11: seq:
          - id: unused
            type: u2
          - id: ip_header
            type: seq
            seq:
              - id: src_ip_addr
                type: u4
              - id: dst_ip_addr
                type: u4
      else: type: u4
    else: type: u4