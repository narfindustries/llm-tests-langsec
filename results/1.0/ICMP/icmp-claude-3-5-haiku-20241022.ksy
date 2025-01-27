meta:
  id: icmp
  title: ICMP (Internet Control Message Protocol) Packet Structure
  endian: be
seq:
  - id: type
    type: u1
    doc: ICMP message type
  - id: code
    type: u1
    doc: ICMP message subtype/code
  - id: checksum
    type: u2
    doc: Checksum of ICMP packet
  - id: rest_of_header
    type: u4
    doc: Remaining header information depending on type
  - id: payload
    type: str
    size-eos: true
    encoding: ascii
    doc: Optional payload data
enums:
  icmp_type:
    0: echo_reply
    3: destination_unreachable
    8: echo_request
    11: time_exceeded