meta:
  id: mqtt
  title: MQTT Protocol
  file-extension: mqtt
  endian: be

seq:
  - id: packet
    type: packet
    repeat: eos

types:
  packet:
    seq:
      - id: fixed_header
        type: fixed_header
      - id: variable_header
        type: variable_header
        size: fixed_header.remaining_length.value_decoded
        if: fixed_header.packet_type != 13 and fixed_header.packet_type != 14
      - id: payload
        type: payload
        size: fixed_header.remaining_length.value_decoded - variable_header._size
        if: fixed_header.packet_type != 13 and fixed_header.packet_type != 14

  fixed_header:
    seq:
      - id: packet_type
        type: u1
      - id: flags
        type: u1
      - id: remaining_length
        type: vlq_base128

  variable_header:
    seq:
      - id: connect_flags
        type: connect_flags
        if: _parent.fixed_header.packet_type == 1
      - id: connack_flags
        type: connack_flags
        if: _parent.fixed_header.packet_type == 2
      - id: publish_flags
        type: publish_flags
        if: _parent.fixed_header.packet_type == 3
      - id: packet_identifier
        type: u2
        if: _parent.fixed_header.packet_type in [4, 5, 6, 7, 8, 9, 10, 11]
      - id: reason_code
        type: u1
        if: _parent.fixed_header.packet_type in [2, 4, 5, 6, 7, 9, 11, 14, 15]
      - id: properties
        type: properties
        size-eos: true
        if: _parent.fixed_header.packet_type in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 14, 15]

  connect_flags:
    seq:
      - id: flags
        type: u1

  connack_flags:
    seq:
      - id: session_present
        type: u1

  publish_flags:
    seq:
      - id: topic_name
        type: str
        encoding: utf-8
      - id: packet_identifier
        type: u2
        if: (_parent.fixed_header.flags & 0x06) != 0

  properties:
    seq:
      - id: length
        type: vlq_base128
      - id: property
        type: property
        repeat: eos

  property:
    seq:
      - id: identifier
        type: u1
      - id: value
        type: property_value

  property_value:
    seq:
      - id: value
        type:
          switch-on: _parent.identifier
          cases:
            0x01: u4
            0x02: u2
            0x03: u1
            0x08: str
            0x09: str
            0x0B: u1
            0x11: u4
            0x12: u2
            0x13: u1
            0x15: u4
            0x16: u2
            0x17: u1
            0x19: u4
            0x1A: u2
            0x1B: u1
            0x21: u4
            0x22: u2
            0x23: u1
            0x24: u4
            0x25: u2
            0x26: u1
            0x27: u4
            0x28: u2
            0x29: u1
            0x31: u4
            0x32: u2
            0x33: u1
            0x34: u4
            0x35: u2
            0x36: u1
            0x37: u4
            0x38: u2
            0x39: u1

  payload:
    seq:
      - id: data
        type: str
        encoding: utf-8
        size-eos: true
        if: _parent.fixed_header.packet_type == 1 or _parent.fixed_header.packet_type == 3 or _parent.fixed_header.packet_type == 8 or _parent.fixed_header.packet_type == 10
      - id: data
        type: bytes
        size-eos: true
        if: _parent.fixed_header.packet_type == 4 or _parent.fixed_header.packet_type == 5 or _parent.fixed_header.packet_type == 6 or _parent.fixed_header.packet_type == 7 or _parent.fixed_header.packet_type == 9 or _parent.fixed_header.packet_type == 11 or _parent.fixed_header.packet_type == 14 or _parent.fixed_header.packet_type == 15

  vlq_base128:
    seq:
      - id: value
        type: u1
        repeat: until
        repeat-until: (value & 0x80) == 0
    instances:
      value_decoded:
        value: (value & 0x7F) << (7 * index)