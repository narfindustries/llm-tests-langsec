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
        17: address_mask_request
        18: address_mask_reply
types:
  echo_reply:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
  echo_request:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
  destination_unreachable:
    seq:
      - id: unused
        type: u4
      - id: original_datagram
        type: original_datagram
  source_quench:
    seq:
      - id: unused
        type: u4
      - id: original_datagram
        type: original_datagram
  redirect:
    seq:
      - id: gateway_internet_address
        type: u4
      - id: original_datagram
        type: original_datagram
  router_advertisement:
    seq:
      - id: num_addrs
        type: u1
      - id: addr_entry_size
        type: u1
      - id: lifetime
        type: u2
      - id: addresses
        type: router_address
        repeat: expr
        repeat-expr: num_addrs
  router_solicitation:
    seq:
      - id: unused
        type: u4
  time_exceeded:
    seq:
      - id: unused
        type: u4
      - id: original_datagram
        type: original_datagram
  parameter_problem:
    seq:
      - id: pointer
        type: u1
      - id: unused
        type: u3
      - id: original_datagram
        type: original_datagram
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
  information_request:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
  information_reply:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
  address_mask_request:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
      - id: address_mask
        type: u4
  address_mask_reply:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
      - id: address_mask
        type: u4
  original_datagram:
    seq:
      - id: ip_header
        type: ip_header
      - id: data
        size: 8
        type: u1
  router_address:
    seq:
      - id: address
        type: u4
      - id: preference_level
        type: u4
  ip_header:
    seq:
      - id: version_ihl
        type: u1
      - id: tos
        type: u1
      - id: total_length
        type: u2
      - id: identification
        type: u2
      - id: flags_fragment_offset
        type: u2
      - id: ttl
        type: u1
      - id: protocol
        type: u1
      - id: header_checksum
        type: u2
      - id: source_address
        type: u4
      - id: destination_address
        type: u4
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
    17: address_mask_request
    18: address_mask_reply
  icmp_code:
    destination_unreachable:
      0: network_unreachable
      1: host_unreachable
      2: protocol_unreachable
      3: port_unreachable
      4: fragmentation_needed_and_df_set
      5: source_route_failed
      6: destination_network_unknown
      7: destination_host_unknown
      8: source_host_isolated
      9: communication_with_destination_network_administratively_prohibited
      10: communication_with_destination_host_administratively_prohibited
      11: network_unreachable_for_type_of_service
      12: host_unreachable_for_type_of_service
    redirect:
      0: redirect_datagram_for_the_network
      1: redirect_datagram_for_the_host
      2: redirect_datagram_for_the_type_of_service_and_network
      3: redirect_datagram_for_the_type_of_service_and_host
    time_exceeded:
      0: time_to_live_exceeded_in_transit
      1: fragment_reassembly_time_exceeded
    parameter_problem:
      0: pointer_indicates_the_error
      1: missing_a_required_option
      2: bad_length