meta:
  id: mqtt_v5
  title: MQTT Version 5.0 Protocol
  file-extension: mqtt
  endian: be

seq:
  - id: packet_type
    type: b4
  - id: flags
    type: b4
  - id: remaining_length
    type: vlq_base128_le

types:
  connect_packet:
    seq:
      - id: protocol_name
        type: strz
        encoding: UTF-8
        size: 4
      - id: protocol_version
        type: u1
      - id: connect_flags
        type: connect_flags
      - id: keep_alive
        type: u2
      - id: properties
        type: properties
      - id: client_id
        type: prefixed_string
      - id: will_properties
        type: properties
        if: connect_flags.will_flag
      - id: will_topic
        type: prefixed_string
        if: connect_flags.will_flag
      - id: will_payload
        type: prefixed_data
        if: connect_flags.will_flag
      - id: username
        type: prefixed_string
        if: connect_flags.username_flag
      - id: password
        type: prefixed_string
        if: connect_flags.password_flag

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

  properties:
    seq:
      - id: length
        type: vlq_base128_le
      - id: props
        type: property
        repeat: expr
        repeat-expr: length.value

  property:
    seq:
      - id: identifier
        type: u1
      - id: value
        type:
          switch-on: identifier
          cases:
            0x01: u4  # Session Expiry Interval
            0x11: u2  # Receive Maximum
            0x12: u2  # Topic Alias Maximum
            0x15: u4  # Maximum Packet Size
            0x17: u1  # Request Problem Information
            0x19: u1  # Request Response Information
            0x1F: prefixed_string  # Authentication Method
            0x20: prefixed_data  # Authentication Data
            0x21: prefixed_string  # Response Information
            0x22: prefixed_string  # Server Reference
            0x23: prefixed_string  # Reason String
            0x26: utf8_string_pair  # User Properties
            0x27: vlq_base128_le  # Subscription Identifier

  prefixed_string:
    seq:
      - id: length
        type: u2
      - id: value
        type: str
        size: length
        encoding: UTF-8

  prefixed_data:
    seq:
      - id: length
        type: u2
      - id: value
        type: bytes
        size: length

  utf8_string_pair:
    seq:
      - id: key
        type: prefixed_string
      - id: value
        type: prefixed_string

  vlq_base128_le:
    seq:
      - id: groups
        type: b7b1
        repeat: until
        repeat-until: not _.has_next
    instances:
      value:
        value: >-
          groups[0].value
          + (groups.size >= 2 ? groups[1].value << 7 : 0)
          + (groups.size >= 3 ? groups[2].value << 14 : 0)
          + (groups.size >= 4 ? groups[3].value << 21 : 0)

  b7b1:
    seq:
      - id: value
        type: b7
      - id: has_next
        type: b1

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
    15: auth