meta:
  id: icmp
  title: Internet Control Message Protocol
  file-extension: icmp
  xref:
    rfc: 792
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
        'message_type::echo_reply': echo_data
        'message_type::echo_request': echo_data
        'message_type::destination_unreachable': unused_data
        'message_type::source_quench': unused_data
        'message_type::redirect': gateway_address
        'message_type::time_exceeded': unused_data
        'message_type::parameter_problem': parameter_data
        'message_type::timestamp': timestamp_data
        'message_type::timestamp_reply': timestamp_data
        'message_type::information_request': info_data
        'message_type::information_reply': info_data
  - id: data
    type:
      switch-on: type
      cases:
        'message_type::echo_reply': arbitrary_data
        'message_type::echo_request': arbitrary_data
        'message_type::timestamp': timestamp_payload
        'message_type::timestamp_reply': timestamp_payload
        _: error_message_data
types:
  echo_data:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
  unused_data:
    seq:
      - id: unused
        type: u4
  gateway_address:
    seq:
      - id: gateway_ip
        type: u4
  parameter_data:
    seq:
      - id: pointer
        type: u1
      - id: unused
        size: 3
  timestamp_data:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
  info_data:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
  arbitrary_data:
    seq:
      - id: payload
        size-eos: true
  timestamp_payload:
    seq:
      - id: original_timestamp
        type: u4
      - id: receive_timestamp
        type: u4
      - id: transmit_timestamp
        type: u4
  error_message_data:
    seq:
      - id: ip_header
        size: 20
      - id: original_datagram
        size: 8
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