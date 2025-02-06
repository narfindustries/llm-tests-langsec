meta:
  id: icmp
  title: ICMP
  application: internet
  endian: be

seq:
  - id: type
    type: u1
  - id: code
    type: u1
  - id: checksum
    type: u2
  - id: rest_of_header
    size: 4
    type:
      switch-on: type
      cases:
        '0': echo_message
        '3': destination_unreachable
        '4': unused
        '5': redirect
        '8': echo_message
        '11': time_exceeded
        '12': parameter_problem
        '13': timestamp_message
        '14': timestamp_message
        '15': unused
        '16': unused
        '17': address_mask_message
        '18': address_mask_message

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
        type: u2
      - id: mtu
        type: u2

  redirect:
    seq:
      - id: gateway_internet_address
        type: ipv4_address

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

  unused:
    seq:
      - id: unused
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
        value: 'b1 << 24 | b2 << 16 | b3 << 8 | b4'