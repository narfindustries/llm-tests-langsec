meta:
  id: mqtt
  title: MQTT Protocol
  xref:
    oasis: mqtt/v5.0
  license: CC0-1.0
  endian: be

doc: |
  MQTT (Message Queuing Telemetry Transport) is an ISO standard
  (ISO/IEC 20922) publish-subscribe-based messaging protocol.
  It works on top of the TCP/IP protocol. It is designed for
  connections with remote locations where a small code footprint
  is required and/or network bandwidth is limited.

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
        'packet_type_enum::puback': ack_header
        'packet_type_enum::pubrec': ack_header
        'packet_type_enum::pubrel': ack_header
        'packet_type_enum::pubcomp': ack_header
        'packet_type_enum::subscribe': subscribe_header
        'packet_type_enum::suback': suback_header
        'packet_type_enum::unsubscribe': unsubscribe_header
        'packet_type_enum::unsuback': ack_header
        'packet_type_enum::pingreq': void
        'packet_type_enum::pingresp': void
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
      - id: packet_type_and_flags
        type: u1
    instances:
      packet_type:
        value: '(packet_type_and_flags >> 4) & 0x0F'
        enum: packet_type_enum
      flags:
        value: 'packet_type_and_flags & 0x0F'

  connect_header:
    seq:
      - id: protocol_name
        type: mqtt_string
      - id: protocol_level
        type: u1
      - id: connect_flags
        type: connect_flags
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
      - id: packet_id
        type: u2
        if: '(fixed_header.flags & 0x06) != 0'
      - id: properties
        type: properties

  ack_header:
    seq:
      - id: packet_id
        type: u2
      - id: properties
        type: properties

  subscribe_header:
    seq:
      - id: packet_id
        type: u2
      - id: properties
        type: properties

  suback_header:
    seq:
      - id: packet_id
        type: u2
      - id: properties
        type: properties

  unsubscribe_header:
    seq:
      - id: packet_id
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

  connect_flags:
    seq:
      - id: reserved
        type: b1
        valid:
          eq: 0
      - id: clean_start
        type: b1
      - id: will_flag
        type: b1
      - id: will_qos
        type: b2
      - id: will_retain
        type: b1
      - id: password_flag
        type: b1
      - id: username_flag
        type: b1

  properties:
    seq:
      - id: length
        type: vlq
      - id: properties
        type: property
        repeat: expr
        repeat-expr: length.value

  property:
    seq:
      - id: id
        type: u1
      - id: value
        type:
          switch-on: id
          cases:
            1: u1
            2: u4
            3: mqtt_string
            8: mqtt_string
            9: bytes_with_length
            11: vlq
            17: u4
            18: mqtt_string
            19: u2
            21: mqtt_string
            22: bytes_with_length
            23: u1
            24: u4
            25: u1
            26: mqtt_string
            28: mqtt_string
            31: mqtt_string
            33: u2
            34: u2
            35: u2
            36: u1
            37: u1
            38: mqtt_string_pair
            39: u4
            40: u1
            41: u1
            42: u1

  mqtt_string:
    seq:
      - id: length
        type: u2
      - id: value
        type: str
        size: length
        encoding: UTF-8

  mqtt_string_pair:
    seq:
      - id: key
        type: mqtt_string
      - id: value
        type: mqtt_string

  bytes_with_length:
    seq:
      - id: length
        type: u2
      - id: data
        type: u1
        repeat: expr
        repeat-expr: length

  vlq:
    seq:
      - id: bytes
        type: u1
        repeat: until
        repeat-until: '_ & 0x80 == 0'
    instances:
      value:
        value: >
          sum([b & 0x7f << (7 * i) for i, b in enumerate(_.reverse())])

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