meta:
  id: mqtt
  title: MQTT Protocol
  application: MQTT
  xref:
    rfc: 6455
  license: CC0-1.0
seq:
  - id: header
    type: fixed_header
  - id: variable_header
    type:
      switch-on: header.packet_type
      cases:
        'packet_type_enum::connect': connect_header
        'packet_type_enum::publish': publish_header
        'packet_type_enum::puback': ack_header
        'packet_type_enum::pubrec': ack_header
        'packet_type_enum::pubrel': ack_header
        'packet_type_enum::pubcomp': ack_header
        'packet_type_enum::subscribe': subscribe_header
        'packet_type_enum::suback': suback_header
        'packet_type_enum::unsubscribe': unsubscribe_header
        'packet_type_enum::unsuback': ack_header
        'packet_type_enum::pingreq': empty_header
        'packet_type_enum::pingresp': empty_header
        'packet_type_enum::disconnect': disconnect_header
        'packet_type_enum::auth': auth_header
  - id: payload
    type:
      switch-on: header.packet_type
      cases:
        'packet_type_enum::connect': connect_payload
        'packet_type_enum::publish': publish_payload
        'packet_type_enum::subscribe': subscribe_payload
        'packet_type_enum::suback': suback_payload
        'packet_type_enum::unsubscribe': unsubscribe_payload
        'packet_type_enum::auth': auth_payload
types:
  fixed_header:
    seq:
      - id: packet_type_flags
        type: u1
        enum: packet_type_enum
    instances:
      packet_type:
        value: packet_type_flags >> 4
      flags:
        value: packet_type_flags & 0x0F
  connect_header:
    seq:
      - id: protocol_name
        type: utf8_str
      - id: protocol_version
        type: u1
      - id: connect_flags
        type: u1
      - id: keep_alive
        type: u2le
  publish_header:
    seq:
      - id: topic_name
        type: utf8_str
      - id: packet_id
        type: u2le
        if: _parent.header.flags & 0x06
  ack_header:
    seq:
      - id: packet_id
        type: u2le
  subscribe_header:
    seq:
      - id: packet_id
        type: u2le
      - id: properties
        type: properties
  suback_header:
    seq:
      - id: packet_id
        type: u2le
  unsubscribe_header:
    seq:
      - id: packet_id
        type: u2le
      - id: properties
        type: properties
  empty_header:
    seq: []
  disconnect_header:
    seq:
      - id: reason_code
        type: u1
      - id: properties
        type: properties
  auth_header:
    seq:
      - id: reason_code
        type: u1
      - id: properties
        type: properties
  properties:
    seq:
      - id: property_len
        type: vlq_int
      - id: property_data
        type: property_data
        size: property_len
  connect_payload:
    seq:
      - id: client_id
        type: utf8_str
      - id: will_properties
        type: properties
        if: _parent.connect_flags & 0x04
      - id: will_topic
        type: utf8_str
        if: _parent.connect_flags & 0x04
      - id: will_payload
        type: u1
        if: _parent.connect_flags & 0x04
  publish_payload:
    seq:
      - id: data
        size-eos: true
  subscribe_payload:
    seq:
      - id: topics
        type: subscription
        repeat: eos
    types:
      subscription:
        seq:
          - id: topic_filter
            type: utf8_str
          - id: options
            type: u1
  suback_payload:
    seq:
      - id: return_codes
        type: u1
        repeat: eos
  unsubscribe_payload:
    seq:
      - id: topics
        type: utf8_str
        repeat: eos
  auth_payload:
    seq:
      - id: auth_method
        type: utf8_str
  vlq_int:
    seq:
      - id: bytes
        type: u1
        repeat: until
        repeat-until: _ & 0x80 == 0

  property_data:
    seq:
      - id: properties
        size-eos: true

  utf8_str:
    seq:
      - id: length
        type: u2le
      - id: value
        type: str
        size: length
        encoding: UTF-8

enums:
  packet_type_enum:
    1: connect
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
    15: auth