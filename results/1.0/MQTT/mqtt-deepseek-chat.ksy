meta:
  id: mqtt_deepseek_chat
  title: MQTT DeepSeek Chat Protocol
  license: MIT
  endian: be
  ks-version: 0.9

seq:
  - id: fixed_header
    type: fixed_header

  - id: variable_header
    type: variable_header
    if: fixed_header.remaining_length > 0

  - id: payload
    size: fixed_header.remaining_length - variable_header._io.size
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

  variable_header:
    seq:
      - id: protocol_name
        type: str
        size: 2
        encoding: UTF-8

      - id: protocol_version
        type: u1

      - id: connect_flags
        type: u1

      - id: keep_alive
        type: u2

  remaining_length:
    seq:
      - id: length
        type: vlq_base128_le

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