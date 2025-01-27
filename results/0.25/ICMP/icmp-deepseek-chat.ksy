meta:
  id: icmp_deepseek_chat
  title: ICMP DeepSeek Chat Protocol
  license: CC0-1.0
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
    size: _root._io.size - 8
    doc: Data payload of the ICMP message