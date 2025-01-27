meta:
  id: icmp_deepseek_chat
  title: ICMP DeepSeek Chat Protocol
  license: CC0-1.0
  endian: be
  file-extension: icmp
  ks-version: 0.9

seq:
  - id: type
    type: u1
    doc: Type of ICMP message
  - id: code
    type: u1
    doc: Code for the ICMP message type
  - id: checksum
    type: u2
    doc: Checksum for the ICMP header and data
  - id: rest_of_header
    type: u4
    doc: Rest of the ICMP header, content depends on type and code
  - id: data
    type: u4
    doc: Data section of the ICMP message, content depends on type and code

types:
  u1:
    seq:
      - id: value
        type: b1
  u2:
    seq:
      - id: value
        type: b2
  u4:
    seq:
      - id: value
        type: b4