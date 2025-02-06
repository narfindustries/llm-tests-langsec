meta:
  id: icmp
  endian: be
seq:
  - id: type
    type: u1
    enum: icmp_type
    doc: ICMP message type
  - id: code
    type: u1
    doc: ICMP message code
    enum:
      switch-on: type
      cases:
        'icmp_type::destination_unreachable': icmp_code_destination_unreachable
        'icmp_type::redirect': icmp_code_redirect
        'icmp_type::time_exceeded': icmp_code_time_exceeded
        'icmp_type::parameter_problem': icmp_code_parameter_problem
  - id: checksum
    type: u2
    doc: Checksum of the ICMP message
  - id: rest_of_header
    type:
      switch-on: type
      cases:
        'icmp_type::echo_request | icmp_type::echo_reply': echo_header
        'icmp_type::destination_unreachable': destination_unreachable_header
        'icmp_type::source_quench': source_quench_header
        'icmp_type::redirect': redirect_header
        'icmp_type::router_advertisement': router_advertisement_header
        'icmp_type::router_solicitation': router_solicitation_header
        'icmp_type::time_exceeded': time_exceeded_header
        'icmp_type::parameter_problem': parameter_problem_header
        'icmp_type::timestamp_request | icmp_type::timestamp_reply': timestamp_header
        'icmp_type::information_request | icmp_type::information_reply': information_header
  - id: data
    size-eos: true
    doc: ICMP message data
types:
  echo_header:
    seq:
      - id: identifier
        type: u2
        doc: Identifier
      - id: sequence_number
        type: u2
        doc: Sequence number
  destination_unreachable_header:
    seq:
      - id: unused
        type: u4
        doc: Unused, must be zero
  source_quench_header:
    seq:
      - id: unused
        type: u4
        doc: Unused, must be zero
  redirect_header:
    seq:
      - id: gateway_internet_address
        type: u4
        doc: Gateway Internet Address
  router_advertisement_header:
    seq:
      - id: address_size
        type: u1
        doc: Number of addresses advertised
      - id: lifetime
        type: u2
        doc: Maximum time the addresses are valid
      - id: router_addresses
        type: u4
        repeat: expr
        repeat-expr: address_size
        doc: List of router addresses
  router_solicitation_header:
    seq:
      - id: reserved
        type: u4
        doc: Reserved, must be zero
  time_exceeded_header:
    seq:
      - id: unused
        type: u4
        doc: Unused, must be zero
  parameter_problem_header:
    seq:
      - id: pointer
        type: u1
        doc: Pointer indicating the error
      - id: unused
        type: b3
        doc: Unused, must be zero
  timestamp_header:
    seq:
      - id: identifier
        type: u2
        doc: Identifier
      - id: sequence_number
        type: u2
        doc: Sequence number
      - id: originate_timestamp
        type: u4
        doc: Originate timestamp
      - id: receive_timestamp
        type: u4
        doc: Receive timestamp
      - id: transmit_timestamp
        type: u4
        doc: Transmit timestamp
  information_header:
    seq:
      - id: identifier
        type: u2
        doc: Identifier
      - id: sequence_number
        type: u2
        doc: Sequence number
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
    13: timestamp_request
    14: timestamp_reply
    15: information_request
    16: information_reply
  icmp_code_destination_unreachable:
    0: network_unreachable
    1: host_unreachable
    2: protocol_unreachable
    3: port_unreachable
    4: fragmentation_needed_and_df_set
    5: source_route_failed
  icmp_code_redirect:
    0: redirect_for_network
    1: redirect_for_host
    2: redirect_for_type_of_service_and_network
    3: redirect_for_type_of_service_and_host
  icmp_code_time_exceeded:
    0: time_to_live_exceeded_in_transit
    1: fragment_reassembly_time_exceeded
  icmp_code_parameter_problem:
    0: pointer_indicates_the_error