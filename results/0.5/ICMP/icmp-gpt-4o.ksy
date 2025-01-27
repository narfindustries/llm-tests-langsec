meta:
  id: icmp_packet
  title: Internet Control Message Protocol (ICMP)
  application: network
  file-extension: icmp
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
  - id: data
    size-eos: true

enums:
  type:
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