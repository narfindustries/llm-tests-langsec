meta:
  id: icmp
  title: Internet Control Message Protocol
  file-extension: icmp
  endian: be

seq:
  - id: type
    type: u1
    enum: message_type
  - id: code
    type: u1
  - id: checksum
    type: u2
  - id: rest_of_header
    type: u4
  - id: payload
    size-eos: true

enums:
  message_type:
    0: echo_reply
    3: destination_unreachable
    4: source_quench
    5: redirect
    8: echo_request
    11: time_exceeded
    12: parameter_problem
    13: timestamp_request
    14: timestamp_reply
    15: information_request
    16: information_reply
    17: address_mask_request
    18: address_mask_reply

instances:
  identifier:
    pos: 4
    type: u2
    if: >
      type == message_type::echo_reply or
      type == message_type::echo_request or
      type == message_type::timestamp_request or
      type == message_type::timestamp_reply or
      type == message_type::information_request or
      type == message_type::information_reply
  sequence_number:
    pos: 6
    type: u2
    if: >
      type == message_type::echo_reply or
      type == message_type::echo_request or
      type == message_type::timestamp_request or
      type == message_type::timestamp_reply or
      type == message_type::information_request or
      type == message_type::information_reply
  gateway_address:
    pos: 4
    type: u4
    if: type == message_type::redirect
  unused:
    pos: 4
    type: u4
    if: >
      type == message_type::destination_unreachable or
      type == message_type::time_exceeded or
      type == message_type::parameter_problem
  pointer:
    pos: 4
    type: u1
    if: type == message_type::parameter_problem
  originate_timestamp:
    pos: 4
    type: u4
    if: >
      type == message_type::timestamp_request or
      type == message_type::timestamp_reply
  receive_timestamp:
    pos: 8
    type: u4
    if: >
      type == message_type::timestamp_request or
      type == message_type::timestamp_reply
  transmit_timestamp:
    pos: 12
    type: u4
    if: >
      type == message_type::timestamp_request or
      type == message_type::timestamp_reply