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
    doc: Depends on the ICMP message type

  - id: payload
    type: bytes
    size-eos: true
    doc: Optional payload data

enums:
  icmp_type:
    0: echo_reply
    3: destination_unreachable
    8: echo_request
    11: time_exceeded

instances:
  is_valid_checksum:
    value: >-
      (checksum == _calculate_checksum())
    doc: Validates the ICMP checksum

  payload_length:
    value: payload.size
    doc: Length of the payload in bytes

  message_type_name:
    value: >-
      (type == 0 ? "Echo Reply" :
       type == 3 ? "Destination Unreachable" :
       type == 8 ? "Echo Request" :
       type == 11 ? "Time Exceeded" : 
       "Unknown")
    doc: Human-readable message type name