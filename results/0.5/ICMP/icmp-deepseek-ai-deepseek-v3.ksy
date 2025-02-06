meta:
  id: icmp
  title: ICMP (Internet Control Message Protocol)
  file-extension: icmp
  license: MIT
  endian: be
seq:
  - id: type
    type: u1
    enum: icmp_type
  - id: code
    type: u1
  - id: checksum
    type: u2
  - id: rest_of_header
    type: rest_of_header
    if: type == 'echo_request' or type == 'echo_reply' or type == 'timestamp_request' or type == 'timestamp_reply' or type == 'information_request' or type == 'information_reply' or type == 'address_mask_request' or type == 'address_mask_reply' or type == 'redirect' or type == 'parameter_problem'
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
    13: timestamp_request
    14: timestamp_reply
    15: information_request
    16: information_reply
    17: address_mask_request
    18: address_mask_reply
types:
  rest_of_header:
    seq:
      - id: identifier
        type: u2
        if: _root.type == 'echo_request' or _root.type == 'echo_reply' or _root.type == 'timestamp_request' or _root.type == 'timestamp_reply' or _root.type == 'information_request' or _root.type == 'information_reply' or _root.type == 'address_mask_request' or _root.type == 'address_mask_reply'
      - id: sequence_number
        type: u2
        if: _root.type == 'echo_request' or _root.type == 'echo_reply' or _root.type == 'timestamp_request' or _root.type == 'timestamp_reply' or _root.type == 'information_request' or _root.type == 'information_reply' or _root.type == 'address_mask_request' or _root.type == 'address_mask_reply'
      - id: gateway_internet_address
        type: u4
        if: _root.type == 'redirect'
      - id: pointer
        type: u1
        if: _root.type == 'parameter_problem'
      - id: timestamp
        type: u4
        if: _root.type == 'timestamp_request' or _root.type == 'timestamp_reply'
      - id: address_mask
        type: u4
        if: _root.type == 'address_mask_request' or _root.type == 'address_mask_reply'
instances:
  data:
    size-eos: true