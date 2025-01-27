meta:
  id: mqtt
  title: MQTT Protocol
  file-extension: mqtt
  endian: be
  license: CC0-1.0
doc: |
  MQTT (Message Queuing Telemetry Transport) is an ISO standard
  (ISO/IEC PRF 20922) publish-subscribe-based messaging protocol.
  It works on top of the TCP/IP protocol and is designed for
  connections with remote locations where a "small code footprint"
  is required or the network bandwidth is limited.

seq:
  - id: header
    type: header

  - id: payload
    size-eos: true
    type:
      switch-on: header.message_type
      cases:
        'message_type::connect': connect_payload
        'message_type::connack': connack_payload
        'message_type::publish': publish_payload
        'message_type::subscribe': subscribe_payload
        'message_type::suback': suback_payload
        'message_type::unsubscribe': unsubscribe_payload
        'message_type::unsuback': unsuback_payload
        'message_type::pingreq': empty_payload
        'message_type::pingresp': empty_payload
        'message_type::disconnect': empty_payload

types:
  header:
    seq:
      - id: message_type
        type: u1
        enum: message_type
      - id: flags
        type: u1
      - id: remaining_length
        type: vlq

  connect_payload:
    seq:
      - id: protocol_name
        type: mqtt_string
      - id: protocol_version
        type: u1
      - id: connect_flags
        type: u1
      - id: keep_alive
        type: u2
      - id: client_id
        type: mqtt_string
      - id: will_topic
        type: mqtt_string
        if: connect_flags & 0b00000100 != 0
      - id: will_message
        type: mqtt_string
        if: connect_flags & 0b00000100 != 0
      - id: username
        type: mqtt_string
        if: connect_flags & 0b10000000 != 0
      - id: password
        type: mqtt_string
        if: connect_flags & 0b01000000 != 0

  connack_payload:
    seq:
      - id: session_present_flag
        type: b1
      - id: return_code
        type: u1

  publish_payload:
    seq:
      - id: topic_name
        type: mqtt_string
      - id: message_id
        type: u2
        if: header.flags & 0b00000110 != 0
      - id: payload
        size-eos: true

  subscribe_payload:
    seq:
      - id: message_id
        type: u2
      - id: subscriptions
        type: subscription
        repeat: eos

  subscription:
    seq:
      - id: topic_filter
        type: mqtt_string
      - id: qos
        type: u1

  suback_payload:
    seq:
      - id: message_id
        type: u2
      - id: return_codes
        type: u1
        repeat: eos

  unsubscribe_payload:
    seq:
      - id: message_id
        type: u2
      - id: topic_filters
        type: mqtt_string
        repeat: eos

  unsuback_payload:
    seq:
      - id: message_id
        type: u2

  empty_payload:
    seq: []

enums:
  message_type:
    1: connect
    2: connack
    3: publish
    4: subscribe
    5: suback
    6: unsubscribe
    7: unsuback
    8: pingreq
    9: pingresp
    10: disconnect

  mqtt_string:
    seq:
      - id: length
        type: u2
      - id: value
        type: str
        size: length
        encoding: UTF-8

  vlq:
    seq:
      - id: bytes
        type: u1
        repeat: until
        repeat-until: _.val & 0x80 == 0
    instances:
      value:
        value: |
          _.bytes.reduce((acc, val) => (acc << 7) | (val & 0x7f), 0)