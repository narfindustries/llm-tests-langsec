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
        'message_type::parameter_problem': parameter_problem_data
        'message_type::redirect': redirect_data
        'message_type::timestamp': timestamp_data
        'message_type::timestamp_reply': timestamp_data
        _: u4
  - id: data
    size-eos: true
types:
  echo_data:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
  parameter_problem_data:
    seq:
      - id: pointer
        type: u1
      - id: unused
        size: 3
  redirect_data:
    seq:
      - id: gateway_internet_address
        type: u4
  timestamp_data:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
      - id: originate_timestamp
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
    13: timestamp
    14: timestamp_reply
    15: information_request
    16: information_reply
  destination_unreachable_code:
    0: net_unreachable
    1: host_unreachable
    2: protocol_unreachable
    3: port_unreachable
    4: fragmentation_needed
    5: source_route_failed
  redirect_code:
    0: network
    1: host
    2: tos_and_network
    3: tos_and_host
  time_exceeded_code:
    0: ttl_exceeded
    1: fragment_reassembly_time_exceeded