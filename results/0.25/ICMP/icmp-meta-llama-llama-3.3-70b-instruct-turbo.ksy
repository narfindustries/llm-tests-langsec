meta:
  id: icmp
  endian: be
seq:
  - id: type
    type: u1
  - id: code
    type: u1
  - id: checksum
    type: u2
  - id: rest_of_header
    type: seq
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
    if: type != 0 and type != 8
  - id: gateway_address
    type: seq
    seq:
      - id: gateway_address_bytes
        type: u4
    if: type == 5
  - id: mtu
    type: seq
    seq:
      - id: mtu_bytes
        type: u2
    if: type == 3 and code == 4
  - id: pointer
    type: seq
    seq:
      - id: pointer_bytes
        type: u1
    if: type == 12
  - id: unused
    type: seq
    seq:
      - id: unused_bytes
        type: u1
    if: type == 11 and code == 1
  - id: data
    type: switch-on type
    cases:
      0:
        type: seq
        seq:
          - id: identifier
            type: u2
          - id: sequence_number
            type: u2
      8:
        type: seq
        seq:
          - id: identifier
            type: u2
          - id: sequence_number
            type: u2
      13:
        type: seq
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
      14:
        type: seq
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
      15:
        type: seq
        seq:
          - id: identifier
            type: u2
          - id: sequence_number
            type: u2
      16:
        type: seq
        seq:
          - id: identifier
            type: u2
          - id: sequence_number
            type: u2
      else:
        type: rest
        size: eos