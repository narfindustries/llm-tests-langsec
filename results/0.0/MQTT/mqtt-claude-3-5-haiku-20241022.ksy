meta:
  id: mqtt
  title: MQTT Protocol Specification
  file-extension: mqtt
  endian: be

seq:
  - id: header
    type: header
  - id: payload
    type: payload

types:
  header:
    seq:
      - id: packet_type
        type: b4
        enum: packet_types
      - id: flags
        type: b4
      - id: remaining_length
        type: vlq_base128_le

  payload:
    seq:
      - id: client_id
        type: str
        size: _root.header.remaining_length
        encoding: utf-8
      - id: optional_fields
        type: optional_fields
        if: _root.header.packet_type == packet_types::connect

  optional_fields:
    seq:
      - id: will_topic
        type: str
        size-eos: true
        encoding: utf-8
        if: _root.header.flags & 0x04 != 0
      - id: will_message
        type: str
        size-eos: true
        encoding: utf-8
        if: _root.header.flags & 0x04 != 0

enums:
  packet_types:
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