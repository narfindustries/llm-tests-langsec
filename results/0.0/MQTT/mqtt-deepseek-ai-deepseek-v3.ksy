meta:
  id: mqtt
  title: MQTT Version 5.0
  license: CC0-1.0
  endian: be
seq:
  - id: fixed_header
    type: fixed_header
  - id: variable_header
    type: variable_header
    if: fixed_header.packet_type != 12 and fixed_header.packet_type != 13
  - id: payload
    type: payload
    if: fixed_header.packet_type == 1 or fixed_header.packet_type == 3 or fixed_header.packet_type == 8 or fixed_header.packet_type == 10
types:
  fixed_header:
    seq:
      - id: packet_type
        type: u1
        enum: packet_types
      - id: flags
        type: u1
      - id: remaining_length
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
        15: AUTH
  variable_header:
    seq:
      - id: connect
        type: connect_vh
        if: _root.fixed_header.packet_type == 1
      - id: connack
        type: connack_vh
        if: _root.fixed_header.packet_type == 2
      - id: publish
        type: publish_vh
        if: _root.fixed_header.packet_type == 3
      - id: puback
        type: puback_vh
        if: _root.fixed_header.packet_type == 4
      - id: pubrec
        type: pubrec_vh
        if: _root.fixed_header.packet_type == 5
      - id: pubrel
        type: pubrel_vh
        if: _root.fixed_header.packet_type == 6
      - id: pubcomp
        type: pubcomp_vh
        if: _root.fixed_header.packet_type == 7
      - id: subscribe
        type: subscribe_vh
        if: _root.fixed_header.packet_type == 8
      - id: suback
        type: suback_vh
        if: _root.fixed_header.packet_type == 9
      - id: unsubscribe
        type: unsubscribe_vh
        if: _root.fixed_header.packet_type == 10
      - id: unsuback
        type: unsuback_vh
        if: _root.fixed_header.packet_type == 11
      - id: disconnect
        type: disconnect_vh
        if: _root.fixed_header.packet_type == 14
      - id: auth
        type: auth_vh
        if: _root.fixed_header.packet_type == 15
  connect_vh:
    seq:
      - id: protocol_name
        type: str_utf8
      - id: protocol_level
        type: u1
      - id: connect_flags
        type: u1
      - id: keep_alive
        type: u2
      - id: properties
        type: properties
  connack_vh:
    seq:
      - id: session_present
        type: u1
      - id: reason_code
        type: u1
      - id: properties
        type: properties
  publish_vh:
    seq:
      - id: topic_name
        type: str_utf8
      - id: packet_identifier
        type: u2
        if: _root.fixed_header.flags & 0x06 != 0
      - id: properties
        type: properties
  puback_vh:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties
  pubrec_vh:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties
  pubrel_vh:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties
  pubcomp_vh:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties
  subscribe_vh:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties
  suback_vh:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_codes
        type: u1
        repeat: eos
      - id: properties
        type: properties
  unsubscribe_vh:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties
  unsuback_vh:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_codes
        type: u1
        repeat: eos
      - id: properties
        type: properties
  disconnect_vh:
    seq:
      - id: reason_code
        type:极客时间
        type: u1
      - id: properties
        type: properties
  auth_vh:
    seq:
      - id: reason_code
        type: u1
      - id: properties
        type: properties
  payload:
    seq:
      - id: connect
        type: connect_payload
        if: _root.fixed_header.packet_type == 1
      - id: publish
        type: publish_payload
        if: _root.fixed_header.packet_type == 3
      - id: subscribe
        type: subscribe_payload
        if: _root.fixed_header.packet_type == 8
      - id: unsubscribe
        type: unsubscribe_payload
        if: _root.fixed_header.packet_type == 10
  connect_payload:
    seq:
      - id: client_identifier
        type: str_极客时间
        type: str_utf8
      - id: will_topic
        type: str_utf8
        if: _root.variable_header.connect.connect_flags & 0x04 != 0
      - id: will_payload
        type: bytes
        size-eos: true
        if: _root.variable_header.connect.connect_flags & 0x04 != 0
      - id: username
        type: str_utf8
        if: _root.variable_header.connect.connect_flags & 0x80 != 0
      - id: password
        type: bytes
        size-eos: true
        if: _root.variable_header.connect.connect_flags & 0x40 != 0
  publish_payload:
    seq:
      - id: data
        type: bytes
        size-eos: true
  subscribe_payload:
    seq:
      - id: topic_filters
        type: topic_filter
        repeat: eos
  unsubscribe_payload:
    seq:
      - id: topic_filters
        type: str_utf8
        repeat: eos
  topic_filter:
    seq:
      - id: filter
        type: str_utf8
      - id: options
        type: u1
  properties:
    seq:
      - id: properties
        type: property
        repeat: eos
  property:
    seq:
      - id: identifier
        type: u1
        enum: property_identifiers
      - id: value
        type:
          switch-on: identifier
          cases:
            0x11: u4
            0x21: u2
            0x27: u4
            0x22: u2
            0x19: u1
            0x1A: u1
            0x1C: str_utf8
            0x26: str_utf8
            0x15: str_utf8
            0x16: bytes
            0x17: str_utf8
            0x18: bytes
            0x1F: str_utf8
            0x23: u2
            0x24: str_utf8
            0x25: str_utf8
            0x28: str_utf8
            0x29: str_utf8
            0x2A: str_utf8
            0x2B: str_utf8
            0x2C: str_utf8
            0x2D: str_utf8
            0x2E: str_utf8
            0x2F: str_utf8
            0x30: str_utf8
            0x31: str