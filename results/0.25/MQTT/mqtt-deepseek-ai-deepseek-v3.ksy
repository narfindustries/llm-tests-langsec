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
    if: fixed_header.remaining_length > 0
  - id: payload
    type: payload
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
        repeat-until: _io.pos >= _io.size or (this.bytes & 0x80) == 0
  variable_header:
    seq:
      - id: packet_type_specific
        type:
          switch-on: _root.fixed_header.packet_type
          cases:
            1: connect_variable_header
            2: connack_variable_header
            3: publish_variable_header
            4: puback_variable_header
            5: pubrec_variable_header
            6: pubrel_variable_header
            7: pubcomp_variable_header
            8: subscribe_variable_header
            9: suback_variable_header
            10: unsubscribe_variable_header
            11: unsuback_variable_header
            14: disconnect_variable_header
            15: auth_variable_header
  connect_variable_header:
    seq:
      - id: protocol_name
        type: strz
        encoding: UTF-8
      - id: protocol_level
        type: u1
      - id: connect_flags
        type: u1
      - id: keep_alive
        type: u2
      - id: properties
        type: properties
  connack_variable_header:
    seq:
      - id: session_present
        type: u1
      - id: reason_code
        type: u1
      - id: properties
        type: properties
  publish_variable_header:
    seq:
      - id: topic_name
        type: strz
        encoding: UTF-8
      - id: packet_identifier
        type: u2
        if: _root.fixed_header.flags & 0x06 != 0
      - id: properties
        type: properties
  puback_variable_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties
  pubrec_variable_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties
  pubrel_variable_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties
  pubcomp_variable_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties
  subscribe_variable_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties
  suback_variable_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_codes
        type: u1
        repeat: until
        repeat-until: _io.pos >= _io.size
      - id: properties
        type: properties
  unsubscribe_variable_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties
  unsuback_variable_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_codes
        type: u1
        repeat: until
        repeat-until: _io.pos >= _io.size
      - id: properties
        type: properties
  disconnect_variable_header:
    seq:
      - id: reason_code
        type: u1
      - id: properties
        type: properties
  auth_variable_header:
    seq:
      - id: reason_code
        type: u1
      - id: properties
        type: properties
  properties:
    seq:
      - id: length
        type: remaining_length
      - id: properties
        type: property
        repeat: until
        repeat-until: _io.pos >= _io.size
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
            0x22: u4
            0x23: u2
            0x24: strz
            0x25: strz
            0x26: strz
            0x27: strz
            0x28: strz
            0x29: strz
            0x2A: strz
            0x2B: strz
            0x2C: strz
            0x2D: strz
            0x2E: strz
            0x2F: strz
            0x30: strz
            0x31: strz
            0x32: strz
            0x33: strz
            0x34: strz
            0x35: strz
            0x36: strz
            0x37: strz
            0x38: strz
            0x39: strz
            0x3A: strz
            0x3B: strz
            0x3C: strz
            0x3D: strz
            0x3E: strz
            0x3F: strz
            0x40: strz
            0x41: strz
            0x42: strz
            0x43: strz
            0x44: strz
            极速版
            0x45: strz
            0x46: strz
            0x47: strz
            0x48: strz
            0x49: strz
            0x4A: strz
            0x4B: strz
            0x4C: strz
            0x4D: strz
            0极速版
            0x4E: strz
            0x4F: strz
            0x50: strz
            0x51: strz
            0x52: strz
            0x53: strz
            0x54: strz
            0x55: strz
            0x56: strz
            0x57: strz
            0x58: strz
            0x59: strz
            0x5A: strz
            0x5B: strz
            0x5C: strz
            0x5D: strz
            0x5E: strz
            0x5F: strz
            0x60: strz
            0x61: strz
            0x62: strz
            0x63: strz
            0x64: strz
            0x65: strz
            0x66: strz
            0x67: strz
            0x68: strz
            0x69: str极速版
            0x6A: strz
            0x6B: strz
            0x6C: strz
            0x6D: strz
            0x6E: strz
            0x6F: strz
            0x70: strz
            0x71: strz
            0x72: strz
            0x73: strz
            0x74: strz
            0x75: strz
            0x76: strz
            0x77: strz
            0x78: strz
            0x79: strz
            0x7A: strz
            0x7B: strz
            0x7C: strz
            0x7D: strz
            0x7E: strz
            0x7F: strz
  payload:
    seq:
      - id: data
        type:
          switch-on: _root.fixed_header.packet_type
          cases:
            1: connect_payload
            3: publish_payload
            8: subscribe_payload
            10: unsubscribe_payload
  connect_payload:
    seq:
      - id: client_id
        type: strz
        encoding: UTF-8
      - id: will_topic
        type: strz
        encoding: UTF-8
        if: _root.variable_header.packet_type_specific.connect_flags & 