meta:
  id: icmp_deepseek_chat
  title: ICMP DeepSeek Chat Protocol
  license: CC0-1.0
  ks-version: 0.9
  endian: be

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
    type: payload
    doc: Payload data

types:
  payload:
    seq:
      - id: payload_data
        type: u1
        repeat: eos
        doc: Payload data of the ICMP message