meta:
  id: icmp
  title: ICMP (Internet Control Message Protocol)
  application: network
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
    size: 4
  - id: body
    size-eos: true
    type:
      switch-on: type
      cases:
        '0': echo_reply
        '3': destination_unreachable
        '4': source_quench
        '5': redirect
        '8': echo_request
        '11': time_exceeded
        '12': parameter_problem
        '13': timestamp_request
        '14': timestamp_reply
        '15': information_request
        '16': information_reply
        '17': address_mask_request
        '18': address_mask_reply
types:
  echo_reply:
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
        size: 4
      - id: data
        size-eos: true
  source_quench:
    seq:
      - id: unused
        size: 4
      - id: data
        size-eos: true
  redirect:
    seq:
      - id: gateway_internet_address
        type: ipv4_address
      - id: data
        size-eos: true
  echo_request:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
      - id: data
        size-eos: true
  time_exceeded:
    seq:
      - id: unused
        size: 4
      - id: data
        size-eos: true
  parameter_problem:
    seq:
      - id: pointer
        type: u1
      - id: unused
        size: 3
      - id: data
        size-eos: true
  timestamp_request:
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
      - id: data
        size-eos: true
  information_reply:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
      - id: data
        size-eos: true
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