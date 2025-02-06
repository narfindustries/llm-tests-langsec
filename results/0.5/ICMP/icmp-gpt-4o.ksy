meta:
  id: icmp_packet
  title: ICMP Packet
  application: internet
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
        0: echo_message
        8: echo_message
        3: destination_unreachable
        11: time_exceeded
        12: parameter_problem
        5: redirect
        13: timestamp_message
        14: timestamp_message
        17: address_mask_message
        18: address_mask_message

types:
  echo_message:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2

  destination_unreachable:
    seq:
      - id: unused
        type: u4

  time_exceeded:
    seq:
      - id: unused
        type: u4

  parameter_problem:
    seq:
      - id: pointer
        type: u1
      - id: unused
        size: 3

  redirect:
    seq:
      - id: gateway_internet_address
        type: ipv4_address

  timestamp_message:
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

  address_mask_message:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
      - id: address_mask
        type: u4

  ipv4_address:
    seq:
      - id: b1
        type: u1
      - id: b2
        type: u1
      - id: b3
        type: u1
      - id: b4
        type: u1
    instances:
      value:
        value: '(b1 << 24) | (b2 << 16) | (b3 << 8) | b4'