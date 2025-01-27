meta:
  id: mqtt
  endian: le

seq:
  - id: header
    type: header

types:
  header:
    seq:
      - id: byte1
        type: u1
      - id: remaining_length
        type: u1

    instances:
      packet_type:
        value: byte1 & 0xf0
      dup:
        value: (byte1 & 0x08) >> 3
      qos:
        value: (byte1 & 0x03) >> 1
      retain:
        value: byte1 & 0x01

      has_remaining_length:
        value: remaining_length != 0

      remaining_length_value:
        type: u1
        if: has_remaining_length
        repeat: expr
        repeat_expr: remaining_length_value & 0x7f != 0