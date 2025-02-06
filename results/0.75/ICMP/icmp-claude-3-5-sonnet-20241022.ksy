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
        'message_type::echo_reply': echo_header
        'message_type::echo_request': echo_header
        'message_type::destination_unreachable': unused_header
        'message_type::source_quench': unused_header
        'message_type::redirect': redirect_header
        'message_type::time_exceeded': unused_header
        'message_type::parameter_problem': parameter_problem_header
        'message_type::timestamp': timestamp_header
        'message_type::timestamp_reply': timestamp_header
        'message_type::information_request': info_header
        'message_type::information_reply': info_header
  - id: data
    type:
      switch-on: type
      cases:
        'message_type::echo_reply': arbitrary_data
        'message_type::echo_request': arbitrary_data
        'message_type::timestamp': timestamp_data
        'message_type::timestamp_reply': timestamp_data
        _: error_data
types:
  echo_header:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
  unused_header:
    seq:
      - id: unused
        type: u4
  redirect_header:
    seq:
      - id: gateway_internet_address
        type: u4
  parameter_problem_header:
    seq:
      - id: pointer
        type: u1
      - id: unused
        size: 3
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
  arbitrary_data:
    seq:
      - id: data
        size-eos: true
  timestamp_data:
    seq:
      - id: original_timestamp
        type: u4
      - id: receive_timestamp
        type: u4
      - id: transmit_timestamp
        type: u4
  error_data:
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
  destination_unreachable_code:
    0: net_unreachable
    1: host_unreachable
    2: protocol_unreachable
    3: port_unreachable
    4: fragmentation_needed_and_df_set
    5: source_route_failed
  redirect_code:
    0: redirect_for_network
    1: redirect_for_host
    2: redirect_for_tos_and_network
    3: redirect_for_tos_and_host
  time_exceeded_code:
    0: time_to_live_exceeded_in_transit
    1: fragment_reassembly_time_exceeded