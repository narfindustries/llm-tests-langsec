meta:
  id: icmp_packet
  title: ICMP (Internet Control Message Protocol) Packet
  xref:
    rfc: 792
  license: CC0-1.0
  endian: be

doc: |
  The Internet Control Message Protocol (ICMP) is used by network devices to send error messages and operational information.

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
        'icmp_type::echo_reply': icmp_echo
        'icmp_type::destination_unreachable': icmp_destination_unreachable
        'icmp_type::source_quench': icmp_unused
        'icmp_type::redirect': icmp_redirect
        'icmp_type::echo': icmp_echo
        'icmp_type::time_exceeded': icmp_time_exceeded
        'icmp_type::parameter_problem': icmp_parameter_problem
        'icmp_type::timestamp': icmp_timestamp
        'icmp_type::timestamp_reply': icmp_timestamp
        'icmp_type::information_request': icmp_echo
        'icmp_type::information_reply': icmp_echo
        'icmp_type::address_mask_request': icmp_address_mask
        'icmp_type::address_mask_reply': icmp_address_mask

types:
  icmp_echo:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
      - id: data
        size-eos: true

  icmp_destination_unreachable:
    seq:
      - id: unused
        type: u4
      - id: original_datagram
        size-eos: true

  icmp_unused:
    seq:
      - id: unused
        type: u4

  icmp_redirect:
    seq:
      - id: gateway_internet_address
        type: u4
      - id: original_datagram
        size-eos: true

  icmp_time_exceeded:
    seq:
      - id: unused
        type: u4
      - id: original_datagram
        size-eos: true

  icmp_parameter_problem:
    seq:
      - id: pointer
        type: u1
      - id: unused
        type: b24
      - id: original_datagram
        size-eos: true

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

  icmp_address_mask:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
      - id: address_mask
        type: u4

enums:
  icmp_type:
    0: echo_reply
    3: destination_unreachable
    4: source_quench
    5: redirect
    8: echo
    11: time_exceeded
    12: parameter_problem
    13: timestamp
    14: timestamp_reply
    15: information_request
    16: information_reply
    17: address_mask_request
    18: address_mask_reply