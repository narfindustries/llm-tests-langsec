meta:
  id: icmp_packet
  title: ICMP (Internet Control Message Protocol) Packet
  license: CC0-1.0
  endian: be

doc: |
  The Internet Control Message Protocol (ICMP) is used by network devices,
  like routers, to send error messages and operational information.

seq:
  - id: type
    type: u1
    enum: icmp_type
  - id: code
    type: u1
  - id: checksum
    type: u2
  - id: rest_of_header
    type:
      switch-on: type
      cases:
        'icmp_type::echo_request': echo
        'icmp_type::echo_reply': echo
        'icmp_type::destination_unreachable': destination_unreachable
        'icmp_type::source_quench': empty
        'icmp_type::redirect': redirect
        'icmp_type::time_exceeded': empty
        'icmp_type::parameter_problem': parameter_problem
        'icmp_type::timestamp_request': timestamp
        'icmp_type::timestamp_reply': timestamp
        'icmp_type::info_request': echo
        'icmp_type::info_reply': echo
        'icmp_type::address_mask_request': address_mask
        'icmp_type::address_mask_reply': address_mask

types:
  echo:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
      - id: data
        size-eos: true

  destination_unreachable:
    seq:
      - id: unused
        type: u4
      - id: next_hop_mtu
        type: u2
      - id: data
        size-eos: true

  redirect:
    seq:
      - id: gateway_internet_address
        type: u4
      - id: data
        size-eos: true

  parameter_problem:
    seq:
      - id: pointer
        type: u1
      - id: unused
        type: b24
      - id: data
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

  address_mask:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
      - id: address_mask
        type: u4

  empty:
    seq: []

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
    15: info_request
    16: info_reply
    17: address_mask_request
    18: address_mask_reply
    30: traceroute
    31: datagram_conversion_error
    32: mobile_host_redirect
    33: ipv6_where_are_you
    34: ipv6_i_am_here
    35: mobile_registration_request
    36: mobile_registration_reply
    37: domain_name_request
    38: domain_name_reply
    39: skip
    40: photuris