meta:
  id: icmp
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
    type: rest_header
    size: 4
  - id: payload
    size-eos: true
    if: not _io.eof

types:
  rest_header:
    seq:
      - id: identifier
        type: u2
        if: _parent.type == message_type::echo_request or _parent.type == message_type::echo_reply
      - id: sequence_number
        type: u2
        if: _parent.type == message_type::echo_request or _parent.type == message_type::echo_reply
      - id: unused
        type: u4
        if: _parent.type != message_type::echo_request and _parent.type != message_type::echo_reply

enums:
  message_type:
    0: echo_reply
    3: destination_unreachable
    4: source_quench
    5: redirect
    8: echo_request
    11: time_exceeded
    12: parameter_problem
    13: timestamp
    14: timestamp_reply
    15: information_request
    16: information_reply