meta:
  id: icmp
  endian: be

seq:
  - id: type
    type: u1
    enum: icmp_type
  - id: code
    type: u1
    enum: icmp_code
  - id: checksum
    type: u2
  - id: rest_of_header
    type:
      switch-on: type
      cases:
        'icmp_type::echo_request': echo_message
        'icmp_type::echo_reply': echo_message
        'icmp_type::destination_unreachable': destination_unreachable
        'icmp_type::time_exceeded': time_exceeded
        'icmp_type::redirect': redirect
        'icmp_type::router_advertisement': router_advertisement
        'icmp_type::router_solicitation': router_solicitation
        'icmp_type::parameter_problem': parameter_problem
        'icmp_type::timestamp': timestamp
        'icmp_type::timestamp_reply': timestamp_reply
        _: generic_header

types:
  echo_message:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
      - id: payload
        type: byte_array
        size-eos: true

  destination_unreachable:
    seq:
      - id: unused
        type: u4
      - id: original_datagram
        type: byte_array
        size-eos: true

  time_exceeded:
    seq:
      - id: unused
        type: u4
      - id: original_datagram
        type: byte_array
        size-eos: true

  redirect:
    seq:
      - id: gateway_address
        type: u4
      - id: original_datagram
        type: byte_array
        size-eos: true

  router_advertisement:
    seq:
      - id: num_addresses
        type: u1
      - id: address_entry_size
        type: u1
      - id: lifetime
        type: u2
      - id: router_addresses
        type: router_address
        repeat: expr
        repeat-expr: num_addresses

  router_address:
    seq:
      - id: address
        type: u4
      - id: preference_level
        type: u4

  router_solicitation:
    seq:
      - id: reserved
        type: u4

  parameter_problem:
    seq:
      - id: pointer
        type: u1
      - id: unused
        type: u3
      - id: original_datagram
        type: byte_array
        size-eos: true

  timestamp:
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

  timestamp_reply:
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

  generic_header:
    seq:
      - id: data
        type: byte_array
        size-eos: true

  byte_array:
    seq:
      - id: data
        type: u1
        repeat: eos

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
    13: timestamp
    14: timestamp_reply
    15: information_request
    16: information_reply

  icmp_code:
    0: net_unreachable
    1: host_unreachable
    2: protocol_unreachable
    3: port_unreachable
    4: fragmentation_needed
    5: source_route_failed
    6: destination_network_unknown
    7: destination_host_unknown
    8: network_administratively_prohibited
    9: host_administratively_prohibited
    10: network_unreachable_for_tos
    11: host_unreachable_for_tos
    12: communication_administratively_prohibited
    13: host_precedence_violation
    14: precedence_cutoff_in_effect
    15: reserved
    16: ttl_expired_in_transit
    17: fragment_reassembly_time_exceeded
    18: redirect_network
    19: redirect_host
    20: redirect_tos_network
    21: redirect_tos_host
    22: pointer_indicates_error
    23: missing_required_option
    24: bad_length