meta:
  id: icmp_packet
  title: ICMP (Internet Control Message Protocol) Packet
  license: CC0-1.0
  endian: be

doc: |
  The Internet Control Message Protocol (ICMP) is used by network devices,
  like routers, to send error messages and operational information indicating,
  for example, that a requested service is not available or that a host or
  router could not be reached.

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
    type:
      switch-on: type
      cases:
        'icmp_type::echo_request': icmp_echo
        'icmp_type::echo_reply': icmp_echo
        'icmp_type::timestamp_request': icmp_timestamp
        'icmp_type::timestamp_reply': icmp_timestamp
        'icmp_type::info_request': icmp_info
        'icmp_type::info_reply': icmp_info
        'icmp_type::dest_unreachable': icmp_dest_unreachable
        'icmp_type::source_quench': icmp_void
        'icmp_type::redirect': icmp_redirect
        'icmp_type::time_exceeded': icmp_void
        'icmp_type::parameter_problem': icmp_parameter_problem
        _: icmp_void

types:
  icmp_echo:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2

  icmp_timestamp:
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

  icmp_info:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2

  icmp_dest_unreachable:
    seq:
      - id: unused
        type: u4

  icmp_void:
    seq: []

  icmp_redirect:
    seq:
      - id: gateway_internet_address
        type: u4

  icmp_parameter_problem:
    seq:
      - id: pointer
        type: u1
      - id: unused
        size: 3

enums:
  icmp_type:
    0: echo_reply
    3: dest_unreachable
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