meta:
  id: icmp_packet
  title: ICMP (Internet Control Message Protocol) Packet
  xref:
    rfc: 792
doc: |
  The Internet Control Message Protocol (ICMP) is used for sending
  error messages and operational information indicating success or failure
  when communicating with another IP address.
  This format applies to IPv4 addressing.

seq:
  - id: type
    type: u1
    enum: icmp_type
  - id: code
    type: u1
  - id: checksum
    type: u2be
  - id: rest_of_header
    type:
      switch-on: type
      cases:
        'icmp_type::echo_request': icmp_echo
        'icmp_type::echo_reply': icmp_echo
        'icmp_type::dest_unreachable': icmp_dest_unreachable
        'icmp_type::source_quench': icmp_empty
        'icmp_type::redirect': icmp_redirect
        'icmp_type::time_exceeded': icmp_empty
        'icmp_type::parameter_problem': icmp_parameter_problem
        'icmp_type::timestamp': icmp_timestamp
        'icmp_type::timestamp_reply': icmp_timestamp
        'icmp_type::info_request': icmp_echo
        'icmp_type::info_reply': icmp_echo
        'icmp_type::address_mask_request': icmp_address_mask
        'icmp_type::address_mask_reply': icmp_address_mask

enums:
  icmp_type:
    0: echo_reply
    3: dest_unreachable
    4: source_quench
    5: redirect
    8: echo_request
    11: time_exceeded
    12: parameter_problem
    13: timestamp
    14: timestamp_reply
    15: info_request
    16: info_reply
    17: address_mask_request
    18: address_mask_reply

types:
  icmp_echo:
    seq:
      - id: identifier
        type: u2be
      - id: sequence_number
        type: u2be
      - id: data
        size-eos: true

  icmp_dest_unreachable:
    seq:
      - id: unused
        type: u4be
      - id: original_data
        size: 64

  icmp_empty:
    seq: []

  icmp_redirect:
    seq:
      - id: gateway_internet_address
        type: u4be
      - id: original_data
        size: 64

  icmp_parameter_problem:
    seq:
      - id: pointer
        type: u1
      - id: unused
        size: 3
      - id: original_data
        size: 64

  icmp_timestamp:
    seq:
      - id: identifier
        type: u2be
      - id: sequence_number
        type: u2be
      - id: originate_timestamp
        type: u4be
      - id: receive_timestamp
        type: u4be
      - id: transmit_timestamp
        type: u4be

  icmp_address_mask:
    seq:
      - id: identifier
        type: u2be
      - id: sequence_number
        type: u2be
      - id: address_mask
        type: u4be