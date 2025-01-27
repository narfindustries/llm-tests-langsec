meta:
  id: mqtt
  file-extension: mqtt
  endian: be

seq:
  - id: fixed_header
    type: fixed_header
  - id: variable_header
    type: variable_header
    if: fixed_header.msg_type != msg_type::disconnect
  - id: payload
    type: payload
    if: fixed_header.msg_type != msg_type::disconnect and fixed_header.msg_type != msg_type::pingreq and fixed_header.msg_type != msg_type::pingresp

types:
  fixed_header:
    seq:
      - id: control_byte
        type: u1
      - id: remaining_length
        type: vlq_int
    instances:
      msg_type:
        value: (control_byte & 0xF0) >> 4
        enum: msg_type
      flags:
        value: control_byte & 0x0F

  variable_header:
    seq:
      - id: protocol_name
        type: mqtt_string
        if: _parent.fixed_header.msg_type == msg_type::connect
      - id: protocol_version
        type: u1
        if: _parent.fixed_header.msg_type == msg_type::connect
      - id: connect_flags
        type: u1
        if: _parent.fixed_header.msg_type == msg_type::connect
      - id: keep_alive
        type: u2
        if: _parent.fixed_header.msg_type == msg_type::connect
      - id: packet_identifier
        type: u2
        if: _parent.fixed_header.msg_type == msg_type::publish or 
            _parent.fixed_header.msg_type == msg_type::puback or
            _parent.fixed_header.msg_type == msg_type::pubrec or
            _parent.fixed_header.msg_type == msg_type::pubrel or
            _parent.fixed_header.msg_type == msg_type::pubcomp or
            _parent.fixed_header.msg_type == msg_type::subscribe or
            _parent.fixed_header.msg_type == msg_type::suback or
            _parent.fixed_header.msg_type == msg_type::unsubscribe or
            _parent.fixed_header.msg_type == msg_type::unsuback

  payload:
    seq:
      - id: client_id
        type: mqtt_string
        if: _parent.fixed_header.msg_type == msg_type::connect
      - id: topic_name
        type: mqtt_string
        if: _parent.fixed_header.msg_type == msg_type::publish
      - id: message
        type: str
        size-eos: true
        encoding: utf8
        if: _parent.fixed_header.msg_type == msg_type::publish

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
        if: byte1 >= 0x80
      - id: byte3
        type: u1
        if: byte2 >= 0x80
      - id: byte4
        type: u1
        if: byte3 >= 0x80
    instances:
      value:
        value: >-
          byte1 & 0x7f +
          (byte2 >= 0x80 ? (byte2 & 0x7f) << 7 : 0) +
          (byte3 >= 0x80 ? (byte3 & 0x7f) << 14 : 0) +
          (byte4 >= 0x80 ? (byte4 & 0x7f) << 21 : 0)

enums:
  msg_type:
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