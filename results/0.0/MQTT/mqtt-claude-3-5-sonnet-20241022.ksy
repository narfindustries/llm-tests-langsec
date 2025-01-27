meta:
  id: mqtt_packet
  file-extension: mqtt
  endian: be

seq:
  - id: fixed_header
    type: fixed_header
  - id: variable_header
    type: variable_header
    if: fixed_header.message_type != message_type::pingreq and fixed_header.message_type != message_type::pingresp
  - id: payload
    type: payload
    if: fixed_header.message_type != message_type::pingreq and fixed_header.message_type != message_type::pingresp and fixed_header.message_type != message_type::disconnect

types:
  fixed_header:
    seq:
      - id: message_type_and_flags
        type: u1
      - id: remaining_length
        type: remaining_length
    instances:
      message_type:
        value: (message_type_and_flags & 0xF0) >> 4
        enum: message_type
      dup_flag:
        value: (message_type_and_flags & 0x08) >> 3
      qos_level:
        value: (message_type_and_flags & 0x06) >> 1
      retain:
        value: message_type_and_flags & 0x01

  remaining_length:
    seq:
      - id: byte
        type: u1
        repeat: until
        repeat-until: (byte & 0x80) == 0
    instances:
      value:
        value: >-
          (byte[0] & 0x7F) +
          (byte[1] & 0x7F) * 128 * (byte.size >= 2 ? 1 : 0) +
          (byte[2] & 0x7F) * 128 * 128 * (byte.size >= 3 ? 1 : 0) +
          (byte[3] & 0x7F) * 128 * 128 * 128 * (byte.size >= 4 ? 1 : 0)

  variable_header:
    seq:
      - id: protocol_name
        type: mqtt_string
        if: _parent.fixed_header.message_type == message_type::connect
      - id: protocol_level
        type: u1
        if: _parent.fixed_header.message_type == message_type::connect
      - id: connect_flags
        type: u1
        if: _parent.fixed_header.message_type == message_type::connect
      - id: keep_alive
        type: u2
        if: _parent.fixed_header.message_type == message_type::connect
      - id: packet_identifier
        type: u2
        if: >-
          _parent.fixed_header.message_type == message_type::publish and
          _parent.fixed_header.qos_level > 0

  mqtt_string:
    seq:
      - id: len
        type: u2
      - id: value
        type: str
        size: len
        encoding: utf8

  payload:
    seq:
      - id: data
        size-eos: true

enums:
  message_type:
    1: connect
    2: connack
    3: publish
    4: puback
    5: pubrec
    6: pubrel
    7: pubcomp
    8: subscribe
    9: suback
    10: unsubscribe
    11: unsuback
    12: pingreq
    13: pingresp
    14: disconnect