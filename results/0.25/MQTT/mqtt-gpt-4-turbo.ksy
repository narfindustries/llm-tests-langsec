meta:
  id: mqtt_packet
  title: MQTT Protocol
  application: MQTT
  xref:
    oasis: mqtt/v5.0
  license: CC0-1.0
  endian: be

doc: |
  MQTT (Message Queuing Telemetry Transport) is a lightweight, publish-subscribe network protocol that transports messages between devices. This spec covers MQTT version 5.0 as defined by OASIS.

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
        'packet_type_enum::unsuback': unsuback_header
        'packet_type_enum::pingreq': empty_header
        'packet_type_enum::pingresp': empty_header
        'packet_type_enum::disconnect': disconnect_header
        'packet_type_enum::auth': auth_header
  - id: payload
    size-eos: true
    type:
      switch-on: fixed_header.packet_type
      cases:
        'packet_type_enum::connect': connect_payload
        'packet_type_enum::publish': publish_payload
        'packet_type_enum::subscribe': subscribe_payload
        'packet_type_enum::suback': suback_payload
        'packet_type_enum::unsubscribe': unsubscribe_payload
        'packet_type_enum::unsuback': empty_payload
        'packet_type_enum::pingreq': empty_payload
        'packet_type_enum::pingresp': empty_payload
        'packet_type_enum::disconnect': empty_payload
        'packet_type_enum::auth': empty_payload

types:
  fixed_header:
    seq:
      - id: packet_type_flags
        type: u1
    instances:
      packet_type:
        value: packet_type_flags >> 4
        enum: packet_type_enum
      flags:
        value: packet_type_flags & 0x0F

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
      - id: properties
        type: properties

  ack_header:
    seq:
      - id: packet_id
        type: u2
      - id: reason_code
        type: u1
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

  unsuback_header:
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

  empty_header:
    seq: []

  empty_payload:
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
            0x01: u2
            0x02: u4
            0x03: u1
            0x04: mqtt_string
            0x05: mqtt_string_pair

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

  connect_flags:
    seq:
      - id: reserved
        type: b1
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

  property_identifier:
    0x01: session_expiry_interval
    0x02: receive_maximum
    0x03: maximum_qos
    0x04: retain_available
    0x05: user_property

  reason_code_enum:
    0x00: success
    0x80: unspecified_error
    0x83: implementation_specific_error
    0x87: not_authorized
    0x90: topic_name_invalid
    0x91: packet_identifier_in_use
    0x97: quota_exceeded
    0x99: payload_format_invalid
    0x9a: retain_not_supported
    0x9c: subscription_identifiers_not_supported
    0x9d: wildcard_subscriptions_not_supported