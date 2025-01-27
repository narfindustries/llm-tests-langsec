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
    if: has_payload

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
      flags:
        value: message_type_and_flags & 0x0F

  remaining_length:
    seq:
      - id: bytes
        type: u1
        repeat: until
        repeat-until: (bytes[-1] & 0x80) == 0
    instances:
      value:
        value: >-
          (bytes[0] & 0x7F) +
          (bytes.size >= 2 ? ((bytes[1] & 0x7F) << 7) : 0) +
          (bytes.size >= 3 ? ((bytes[2] & 0x7F) << 14) : 0) +
          (bytes.size >= 4 ? ((bytes[3] & 0x7F) << 21) : 0)

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
        if: needs_packet_identifier
      - id: return_code
        type: u1
        if: _parent.fixed_header.message_type == message_type::connack
    instances:
      needs_packet_identifier:
        value: >-
          _parent.fixed_header.message_type == message_type::puback or
          _parent.fixed_header.message_type == message_type::pubrec or
          _parent.fixed_header.message_type == message_type::pubrel or
          _parent.fixed_header.message_type == message_type::pubcomp or
          _parent.fixed_header.message_type == message_type::subscribe or
          _parent.fixed_header.message_type == message_type::suback or
          _parent.fixed_header.message_type == message_type::unsubscribe or
          _parent.fixed_header.message_type == message_type::unsuback

  mqtt_string:
    seq:
      - id: len
        type: u2
      - id: value
        type: str
        size: len
        encoding: UTF-8

  payload:
    seq:
      - id: data
        size-eos: true

instances:
  has_payload:
    value: >-
      fixed_header.message_type == message_type::publish or
      fixed_header.message_type == message_type::subscribe or
      fixed_header.message_type == message_type::suback or
      fixed_header.message_type == message_type::unsubscribe

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