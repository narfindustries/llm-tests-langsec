meta:
  id: mqtt_packet
  file-extension: mqtt
  endian: be

seq:
  - id: fixed_header
    type: fixed_header
  - id: variable_header
    type: 
      switch-on: fixed_header.message_type
      cases:
        0x1: connect_variable_header
        0x2: connack_variable_header
        0x3: publish_variable_header
        0x4: puback_variable_header
        0x8: subscribe_variable_header
        0x9: suback_variable_header
        0xc: pingreq_variable_header
        0xd: pingresp_variable_header
  - id: payload
    type:
      switch-on: fixed_header.message_type
      cases:
        0x1: connect_payload
        0x3: publish_payload
        0x8: subscribe_payload
    if: fixed_header.message_type != 0x2 and fixed_header.message_type != 0x4 and fixed_header.message_type != 0x9 and fixed_header.message_type != 0xc and fixed_header.message_type != 0xd

types:
  fixed_header:
    seq:
      - id: message_type
        type: b4
      - id: dup_flag
        type: b1
      - id: qos_level
        type: b2
      - id: retain
        type: b1
      - id: remaining_length
        type: u1

  mqtt_string:
    seq:
      - id: len
        type: u2
      - id: value
        type: str
        size: len
        encoding: utf8

  connect_variable_header:
    seq:
      - id: protocol_name
        type: mqtt_string
      - id: protocol_level
        type: u1
      - id: connect_flags
        type: u1
      - id: keep_alive
        type: u2

  connect_payload:
    seq:
      - id: client_id
        type: mqtt_string
      - id: will_topic
        type: mqtt_string
        if: "(_parent.variable_header.connect_flags & 0x04) != 0"
      - id: will_message
        type: mqtt_string
        if: "(_parent.variable_header.connect_flags & 0x04) != 0"
      - id: username
        type: mqtt_string
        if: "(_parent.variable_header.connect_flags & 0x80) != 0"
      - id: password
        type: mqtt_string
        if: "(_parent.variable_header.connect_flags & 0x40) != 0"

  connack_variable_header:
    seq:
      - id: ack_flags
        type: u1
      - id: return_code
        type: u1

  publish_variable_header:
    seq:
      - id: topic_name
        type: mqtt_string
      - id: packet_identifier
        type: u2
        if: "_parent.fixed_header.qos_level > 0"

  publish_payload:
    seq:
      - id: data
        size-eos: true

  puback_variable_header:
    seq:
      - id: packet_identifier
        type: u2

  subscribe_variable_header:
    seq:
      - id: packet_identifier
        type: u2

  subscribe_payload:
    seq:
      - id: topics
        type: subscribe_topic
        repeat: eos

  subscribe_topic:
    seq:
      - id: topic_filter
        type: mqtt_string
      - id: qos
        type: u1

  suback_variable_header:
    seq:
      - id: packet_identifier
        type: u2

  pingreq_variable_header:
    seq: []

  pingresp_variable_header:
    seq: []