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
    doc: Subtype of the ICMP message type

  - id: checksum
    type: u2
    doc: Checksum of the ICMP message

  - id: rest_of_header
    type: u4
    doc: Depends on ICMP message type and code

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

instances:
  header_length:
    value: 8
    doc: Fixed ICMP header length in bytes

  is_valid_checksum:
    value: checksum != 0
    doc: Simple validation of checksum field