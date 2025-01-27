meta:
  id: icmp
  title: ICMP (Internet Control Message Protocol) Packet Structure
  endian: be
seq:
  - id: type
    type: u1
    enum: icmp_type
    doc: ICMP message type
  - id: code
    type: u1
    doc: Subtype of the message type
  - id: checksum
    type: u2
    doc: Checksum of the ICMP header and data
  - id: rest_of_header
    type: u4
    doc: Depends on ICMP message type (varies by type)
  - id: payload
    type: bytes
    size-eos: true
    doc: Optional payload data

enums:
  icmp_type:
    0: echo_reply
    3: destination_unreachable
    4: source_quench
    5: redirect
    8: echo_request
    9: router_advertisement
    10: router_solicitation
    11: time_exceeded
    12: parameter_problem
    13: timestamp
    14: timestamp_reply
    15: information_request
    16: information_reply
