meta:
  id: mqtt_packet
  title: MQTT Protocol
  xref:
    oasis: MQTT 5.0
  license: CC0-1.0
  endian: be
doc: |
  MQTT (Message Queuing Telemetry Transport) is an ISO standard (ISO/IEC PRF 20922)
  publish-subscribe-based messaging protocol. It works on top of the TCP/IP protocol and is designed for
  connections with remote locations where a small code footprint is required and/or network bandwidth is limited.

seq:
  - id: fixed_header
    type: fixed_header
  - id: variable_header
    type:
      switch-on: fixed_header.packet_type
      cases:
        'packet_type_enum::connect': connect_header
        'packet_type_enum::connack': connack_header
        'packet_type_enum::publish': publish_header
        'packet_type_enum::puback': puback_header
        'packet_type_enum::pubrec': pubrec_header
        'packet_type_enum::pubrel': pubrel_header
        'packet_type_enum::pubcomp': pubcomp_header
        'packet_type_enum::subscribe': subscribe_header
        'packet_type_enum::suback': suback_header
        'packet_type_enum::unsubscribe': unsubscribe_header
        'packet_type_enum::unsuback': unsuback_header
        'packet_type_enum::pingreq': empty_header
        'packet_type_enum::pingresp': empty_header
        'packet_type_enum::disconnect': disconnect_header
        'packet_type_enum::auth': auth_header
  - id: payload
    type:
      switch-on: fixed_header.packet_type
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
    instances:
      packet_type:
        value: packet_type_flags >> 4
      flags:
        value: packet_type_flags & 0x0F
  connect_header:
    seq:
      - id: protocol_name
        type: mqtt_string
      - id: protocol_version
        type: u1
      - id: connect_flags
        type: u1
      - id: keep_alive
        type: u2
      - id: properties
        type: properties
  connack_header:
    seq:
      - id: ack_flags
        type: u1
      - id: reason_code
        type: u1
      - id: properties
        type: properties
  publish_header:
    seq:
      - id: topic_name
        type: mqtt_string
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties
  puback_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties
  pubrec_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties
  pubrel_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties
  pubcomp_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties
  subscribe_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties
  suback_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties
  unsubscribe_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties
  unsuback_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties
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
  empty_header:
    seq: []
  properties:
    seq:
      - id: length
        type: vlq
      - id: property_list
        type: property
        repeat: expr
        repeat-expr: length
  property:
    seq:
      - id: identifier
        type: u1
      - id: value
        type:
          switch-on: identifier
          cases:
            1: u2
            2: mqtt_string
            3: u4
            4: binary_data
  mqtt_string:
    seq:
      - id: length
        type: u2
      - id: value
        type: str
        size: length
        encoding: UTF-8
  binary_data:
    seq:
      - id: length
        type: u2
      - id: value
        type: u1
        repeat: expr
        repeat-expr: length
  vlq:
    seq:
      - id: bytes
        type: u1
        repeat: until
        repeat-until: _ & 0x80 == 0
    instances:
      value:
        value: |
          sum((byte & 0x7f) << (7 * i) for i, byte in enumerate(reversed(self.bytes)))
enums:
  packet_type_enum:
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
    15: auth