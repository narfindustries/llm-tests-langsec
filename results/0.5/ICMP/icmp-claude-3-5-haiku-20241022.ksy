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
    doc: ICMP message code specific to the type

  - id: checksum
    type: u2
    doc: Checksum of the ICMP message

  - id: rest_of_header
    type: u4
    doc: Depends on ICMP message type and code

instances:
  payload:
    pos: 8
    size-eos: true
    doc: Optional payload data after ICMP header

enums:
  icmp_type:
    0: echo_reply
    3: destination_unreachable
    8: echo_request
    11: time_exceeded

types:
  echo_message:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
      - id: payload
        type: str
        encoding: UTF-8
        size-eos: true