meta:
  id: icmp_packet
  title: ICMP Packet
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
    size: _io.size - 4
    type:
      switch-on: type
      cases:
        '0': echo
        '3': destination_unreachable
        '4': source_quench
        '5': redirect
        '8': echo
        '11': time_exceeded
        '12': parameter_problem
        '13': timestamp
        '14': timestamp_reply
        '17': address_mask_request
        '18': address_mask_reply

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
        type: u1
        repeat: expr
        repeat-expr: 4
      - id: original_datagram
        size-eos: true

  source_quench:
    seq:
      - id: unused
        type: u1
        repeat: expr
        repeat-expr: 4
      - id: original_datagram
        size-eos: true

  redirect:
    seq:
      - id: gateway_internet_address
        type: ipv4_address
      - id: original_datagram
        size-eos: true

  time_exceeded:
    seq:
      - id: unused
        type: u1
        repeat: expr
        repeat-expr: 4
      - id: original_datagram
        size-eos: true

  parameter_problem:
    seq:
      - id: pointer
        type: u1
      - id: unused
        type: u1
        repeat: expr
        repeat-expr: 3
      - id: original_datagram
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

  address_mask_request:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
      - id: address_mask
        type: ipv4_address

  address_mask_reply:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
      - id: address_mask
        type: ipv4_address

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