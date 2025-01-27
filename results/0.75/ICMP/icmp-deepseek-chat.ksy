meta:
  id: icmp_deepseek_chat
  title: ICMP DeepSeek Chat Protocol
  license: CC0-1.0
  endian: le

seq:
  - id: type
    type: u1
    doc: Type of ICMP message
  - id: code
    type: u1
    doc: Code for the ICMP message
  - id: checksum
    type: u2
    doc: Checksum for the ICMP message
  - id: rest_of_header
    type: u4
    doc: Rest of the ICMP header
  - id: data
    size-eos: true
    doc: Data payload of the ICMP message

types:
  u1:
    seq:
      - id: value
        type: u1
  u2:
    seq:
      - id: value
        type: u2
  u4:
    seq:
      - id: value
        type: u4