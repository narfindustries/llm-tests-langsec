meta:
  id: mqtt_deepseek_chat
  title: MQTT DeepSeek Chat Protocol
  license: MIT
  ks-version: 0.9
  endian: be
seq:
  - id: fixed_header
    type: fixed_header
  - id: variable_header
    type: variable_header
    if: fixed_header.remaining_length > 0
  - id: payload
    size: fixed_header.remaining_length - _io.pos
    if: fixed_header.remaining_length > 0
types:
  fixed_header:
    seq:
      - id: packet_type
        type: u1
        enum: packet_types
      - id: flags
        type: u1
      - id: remaining_length
        type: remaining_length
  remaining_length:
    seq:
      - id: bytes
        type: u1
        repeat: until
        repeat-until: _ == 0 or _io.eof?
  variable_header:
    seq:
      - id: protocol_name
        type: strz
        encoding: UTF-8
      - id: protocol_version
        type: u1
      - id: connect_flags
        type: u1
      - id: keep_alive
        type: u2
  strz:
    seq:
      - id: length
        type: u2
      - id: value
        type: str
        size: length
        encoding: UTF-8
enums:
  packet_types:
    1: CONNECT
    2: CONNACK
    3: PUBLISH
    4: PUBACK
    5: PUBREC
    6: PUBREL
    7: PUBCOMP
    8: SUBSCRIBE
    9: SUBACK
    10: UNSUBSCRIBE
    11: UNSUBACK
    12: PINGREQ
    13: PINGRESP
    14: DISCONNECT