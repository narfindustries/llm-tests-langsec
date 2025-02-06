meta:
  id: icmp_packet
  title: Internet Control Message Protocol (ICMP)
  xref:
    rfc: 792
  endian: be

doc: |
  The Internet Control Message Protocol (ICMP) is used by network devices to send
  error messages and operational information indicating success or failure when
  communicating with another IP address.

seq:
  - id: type
    type: u1
    enum: type_enum
  - id: code
    type: u1
  - id: checksum
    type: u2
  - id: rest_of_header
    type:
      switch-on: type
      cases:
        'type_enum::echo_reply': icmp_echo
        'type_enum::destination_unreachable': icmp_destination_unreachable
        'type_enum::source_quench': icmp_source_quench
        'type_enum::redirect': icmp_redirect
        'type_enum::echo': icmp_echo
        'type_enum::time_exceeded': icmp_time_exceeded
        'type_enum::parameter_problem': icmp_parameter_problem
        'type_enum::timestamp': icmp_timestamp
        'type_enum::timestamp_reply': icmp_timestamp
        'type_enum::information_request': icmp_echo
        'type_enum::information_reply': icmp_echo
        'type_enum::address_mask_request': icmp_address_mask
        'type_enum::address_mask_reply': icmp_address_mask

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
        size: 64

  icmp_source_quench:
    seq:
      - id: unused
        type: u4
      - id: original_datagram
        size: 64

  icmp_redirect:
    seq:
      - id: gateway_internet_address
        type: u4
      - id: original_datagram
        size: 64

  icmp_time_exceeded:
    seq:
      - id: unused
        type: u4
      - id: original_datagram
        size: 64

  icmp_parameter_problem:
    seq:
      - id: pointer
        type: u1
      - id: unused
        type: u3
        size: 3
      - id: original_datagram
        size: 64

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
  type_enum:
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