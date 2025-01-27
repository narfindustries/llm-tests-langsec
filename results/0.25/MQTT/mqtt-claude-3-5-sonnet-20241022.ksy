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
    size: fixed_header.remaining_length - variable_header._sizeof
    if: fixed_header.remaining_length > 0 and fixed_header.message_type != message_type::pingreq and fixed_header.message_type != message_type::pingresp

types:
  fixed_header:
    seq:
      - id: message_type_and_flags
        type: u1
      - id: remaining_length
        type: vlq_int
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
        if: _parent.fixed_header.message_type == message_type::publish and _parent.fixed_header.qos_level > 0
      - id: topic_name
        type: mqtt_string
        if: _parent.fixed_header.message_type == message_type::publish

  mqtt_string:
    seq:
      - id: len
        type: u2
      - id: value
        type: str
        size: len
        encoding: utf8

  vlq_int:
    seq:
      - id: byte1
        type: u1
      - id: byte2
        type: u1
        if: "byte1 >= 128"
      - id: byte3
        type: u1
        if: "byte2 >= 128"
      - id: byte4
        type: u1
        if: "byte3 >= 128"
    instances:
      value:
        value: >-
          byte1 % 128 +
          (byte2 >= 128 ? ((byte2 % 128) << 7) : 0) +
          (byte3 >= 128 ? ((byte3 % 128) << 14) : 0) +
          (byte4 >= 128 ? ((byte4 % 128) << 21) : 0)

enums:
  message_type:
    0: reserved
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
    15: reserved2