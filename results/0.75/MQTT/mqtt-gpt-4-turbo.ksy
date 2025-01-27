meta:
  id: mqtt
  title: MQTT Protocol
  file-extension: mqtt
  endian: be
  license: CC0-1.0
seq:
  - id: header
    type: header
  - id: payload
    type:
      switch-on: header.message_type
      cases:
        'message_type::connect': connect_payload
        'message_type::connack': connack_payload
        'message_type::publish': publish_payload
        'message_type::puback': puback_payload
        'message_type::pubrec': pubrec_payload
        'message_type::pubrel': pubrel_payload
        'message_type::pubcomp': pubcomp_payload
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
      - id: dup_flag
        type: b1
      - id: qos_level
        type: b2
      - id: retain
        type: b1
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
        if: (connect_flags & 0x04) != 0
      - id: will_message
        type: mqtt_string
        if: (connect_flags & 0x04) != 0
      - id: username
        type: mqtt_string
        if: (connect_flags & 0x80) != 0
      - id: password
        type: mqtt_string
        if: (connect_flags & 0x40) != 0
  connack_payload:
    seq:
      - id: session_present_flag
        type: b1
      - id: return_code
        type: u1
  publish_payload:
    seq:
      - id: topic
        type: mqtt_string
      - id: message_id
        type: u2
        if: _.header.qos_level != 0
      - id: data
        size-eos: true
  puback_payload:
    seq:
      - id: message_id
        type: u2
  pubrec_payload:
    seq:
      - id: message_id
        type: u2
  pubrel_payload:
    seq:
      - id: message_id
        type: u2
  pubcomp_payload:
    seq:
      - id: message_id
        type: u2
  subscribe_payload:
    seq:
      - id: message_id
        type: u2
      - id: topic_filters
        type: topic_filter
        repeat: eos
  topic_filter:
    seq:
      - id: topic
        type: mqtt_string
      - id: requested_qos
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
      - id: topics
        type: mqtt_string
        repeat: eos
  unsuback_payload:
    seq:
      - id: message_id
        type: u2
  empty_payload:
enums:
  message_type:
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
  mqtt_string:
    seq:
      - id: length
        type: u2
      - id: value
        type: str
        size: length
  vlq:
    seq:
      - id: bytes
        type: u1
        repeat: eos
    instances:
      value:
        value: |
          _.bytes.reduce((acc, byte) => (acc << 7) | (byte & 0x7F), 0)