meta:
  id: icmp
  title: Internet Control Message Protocol
  file-format: network-protocol
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
      0: 
        - id: identifier
          type: u2
        - id: sequence_number
          type: u2
      3: 
        - id: unused
          type: u2
        - id: mtu
          type: u2
      8: 
        - id: identifier
          type: u2
        - id: sequence_number
          type: u2
      11: 
        - id: unused
          type: u2
        - id: mtu
          type: u2
      else: 
        - id: body_bytes
          type: bytes
          size: eos