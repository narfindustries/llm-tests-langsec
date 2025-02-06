meta:
  id: icmp
  title: ICMP
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
    type:
      switch-on: type
      cases:
        'message_type::echo_reply': echo_header
        'message_type::echo_request': echo_header
        'message_type::redirect': redirect_header
        'message_type::timestamp_request': timestamp_header
        'message_type::timestamp_reply': timestamp_header
        'message_type::information_request': info_header
        'message_type::information_reply': info_header
        'message_type::parameter_problem': parameter_problem_header
        _: unused_header
  - id: data
    type:
      switch-on: type
      cases:
        'message_type::echo_reply': arbitrary_data
        'message_type::echo_request': arbitrary_data
        'message_type::timestamp_request': timestamp_data
        'message_type::timestamp_reply': timestamp_data
        _: error_data
    size-eos: true
types:
  echo_header:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
  redirect_header:
    seq:
      - id: gateway_internet_address
        type: u4
  timestamp_header:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
  info_header:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
  parameter_problem_header:
    seq:
      - id: pointer
        type: u1
      - id: unused1
        type: u1
      - id: unused2
        type: u1
      - id: unused3
        type: u1
  unused_header:
    seq:
      - id: unused
        type: u4
  arbitrary_data:
    seq:
      - id: data
        size-eos: true
  error_data:
    seq:
      - id: ip_header
        size: 20
      - id: original_datagram
        size: 8
  timestamp_data:
    seq:
      - id: original_timestamp
        type: u4
      - id: receive_timestamp
        type: u4
      - id: transmit_timestamp
        type: u4
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