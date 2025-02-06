meta:
  id: icmp
  title: Internet Control Message Protocol
  license: CC0-1.0
  endian: be

seq:
  - id: type
    type: u1
    enum: icmp_type
  - id: code
    type: u1
  - id: checksum
    type: u2
  - id: rest_of_header
    size: 4
  - id: data
    size-eos: true

enums:
  icmp_type:
    0: echo_reply
    3: destination_unreachable
    4: source_quench
    5: redirect
    8: echo
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

  icmp_code_destination_unreachable:
    0: network_unreachable
    1: host_unreachable
    2: protocol_unreachable
    3: port_unreachable
    4: fragmentation_needed_and_df_set
    5: source_route_failed
    6: destination_network_unknown
    7: destination_host_unknown
    8: source_host_isolated
    9: network_administratively_prohibited
    10: host_administratively_prohibited
    11: network_unreachable_for_tos
    12: host_unreachable_for_tos
    13: communication_administratively_prohibited
    14: host_precedence_violation
    15: precedence_cutoff_in_effect

  icmp_code_redirect:
    0: redirect_datagram_for_network
    1: redirect_datagram_for_host
    2: redirect_datagram_for_tos_and_network
    3: redirect_datagram_for_tos_and_host

  icmp_code_time_exceeded:
    0: ttl_exceeded_in_transit
    1: fragment_reassembly_time_exceeded

  icmp_code_parameter_problem:
    0: pointer_indicates_error
    1: missing_required_option
    2: bad_length

types:
  icmp_code:
    seq:
      - id: code
        type: u1
        enum: switch-on
        cases:
          'icmp_type::destination_unreachable': icmp_code_destination_unreachable
          'icmp_type::redirect': icmp_code_redirect
          'icmp_type::time_exceeded': icmp_code_time_exceeded
          'icmp_type::parameter_problem': icmp_code_parameter_problem