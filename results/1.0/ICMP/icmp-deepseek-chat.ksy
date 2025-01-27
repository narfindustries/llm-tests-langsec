meta:
  id: icmp_deepseek_chat
  title: ICMP DeepSeek Chat Protocol
  license: MIT
  ks-version: 0.9
  endian: be

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
    doc: Optional data, content depends on type and code

types:
  u1:
    doc: Unsigned 1-byte integer
  u2:
    doc: Unsigned 2-byte integer
  u4:
    doc: Unsigned 4-byte integer