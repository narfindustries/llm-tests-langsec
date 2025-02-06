meta:
  id: icmp
  title: Internet Control Message Protocol (ICMP)
  endian: be

seq:
  - id: type
    type: u1
    enum: icmp_type
  - id: code
    type: u1
    doc: "Code depends on type, varies by message"
  - id: checksum
    type: u2
  - id: rest_of_header
    type:
      switch-on: type
      cases:
        'icmp_type::echo_request': echo_message
        'icmp_type::echo_reply': echo_message
        'icmp_type::destination_unreachable': destination_unreachable
        'icmp_type::time_exceeded': time_exceeded
        'icmp_type::redirect': redirect
        'icmp_type::timestamp': timestamp
        'icmp_type::timestamp_reply': timestamp_reply
        '_': raw_rest_of_header

types:
  raw_rest_of_header:
    seq:
      - id: data
        type: u4

  echo_message:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
      - id: payload
        type: b8
        repeat: eos

  destination_unreachable:
    seq:
      - id: unused
        type: u4
      - id: original_datagram
        type: b8
        repeat: eos

  time_exceeded:
    seq:
      - id: unused
        type: u4
      - id: original_datagram
        type: b8
        repeat: eos

  redirect:
    seq:
      - id: gateway_address
        type: u4
      - id: original_datagram
        type: b8
        repeat: eos

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
    13: timestamp
    14: timestamp_reply
    15: information_request
    16: information_reply