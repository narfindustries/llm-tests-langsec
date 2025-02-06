meta:
  id: icmp
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
    type: icmp_rest_of_header
    if: type != 0 and type != 8 and type != 13 and type != 14 and type != 15 and type != 16 and type != 17 and type != 18
  - id: data
    size-eos: true
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
    17: address_mask_request
    18: address_mask_reply
types:
  icmp_rest_of_header:
    seq:
      - id: gateway_internet_address
        type: u4
        if: _parent.type == 5
      - id: identifier
        type: u2
        if: _parent.type == 0 or _parent.type == 8 or _parent.type == 13 or _parent.type == 14
      - id: sequence_number
        type: u2
        if: _parent.type == 0 or _parent.type == 8 or _parent.type == 13 or _parent.type == 14
      - id: originate_timestamp
        type: u4
        if: _parent.type == 13 or _parent.type == 14
      - id: receive_timestamp
        type: u4
        if: _parent.type == 13 or _parent.type == 14
      - id: transmit_timestamp
        type: u4
        if: _parent.type == 13 or _parent.type == 14
      - id: address_mask
        type: u4
        if: _parent.type == 17 or _parent.type == 18