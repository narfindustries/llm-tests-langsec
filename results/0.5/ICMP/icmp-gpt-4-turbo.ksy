meta:
  id: icmp_packet
  title: ICMP (Internet Control Message Protocol) Packet
  license: CC0-1.0
  endian: be

seq:
  - id: type
    type: u1
  - id: code
    type: u1
  - id: checksum
    type: u2
  - id: rest_of_header
    type:
      switch-on: type
      cases:
        0: icmp_echo
        8: icmp_echo
        13: icmp_timestamp
        14: icmp_timestamp
        17: icmp_address_mask
        18: icmp_address_mask
        3: icmp_unreach
        11: icmp_time_exceeded
        5: icmp_redirect
        12: icmp_parameter_problem
        _: icmp_default

types:
  icmp_echo:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
      - id: data
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

  icmp_default:
    seq:
      - id: unused
        type: u4
      - id: data
        size-eos: true

  icmp_unreach:
    seq:
      - id: unused
        type: u4
      - id: data
        size-eos: true

  icmp_time_exceeded:
    seq:
      - id: unused
        type: u4
      - id: data
        size-eos: true

  icmp_redirect:
    seq:
      - id: gateway_internet_address
        type: u4
      - id: data
        size-eos: true

  icmp_parameter_problem:
    seq:
      - id: pointer
        type: u1
      - id: unused
        type: b3
      - id: data
        size-eos: true