meta:
  id: mqtt
  title: MQTT (Message Queuing Telemetry Transport) Protocol
  xref:
    oasis: MQTT 5.0
  license: CC0-1.0
  endian: be
doc: |
  MQTT is a machine-to-machine (M2M)/"Internet of Things" connectivity protocol.
  It was designed as an extremely lightweight publish/subscribe messaging transport.
seq:
  - id: header
    type: fixed_header
  - id: body
    type:
      switch-on: header.packet_type
      cases:
        1: connect_packet
        2: connack_packet
        3: publish_packet
        4: puback_packet
        5: pubrec_packet
        6: pubrel_packet
        7: pubcomp_packet
        8: subscribe_packet
        9: suback_packet
        10: unsubscribe_packet
        11: unsuback_packet
        12: empty_packet
        13: empty_packet
        14: disconnect_packet
        15: auth_packet
types:
  fixed_header:
    seq:
      - id: packet_type_and_flags
        type: u1
    instances:
      packet_type:
        value: packet_type_and_flags >> 4
      flags:
        value: packet_type_and_flags & 0x0F
  connect_packet:
    seq:
      - id: protocol_name
        type: mqtt_string
      - id: protocol_level
        type: u1
      - id: connect_flags
        type: u1
      - id: keep_alive
        type: u2
      - id: properties
        type: property
      - id: client_id
        type: mqtt_string
      - id: will_properties
        type: property
        if: connect_flags & 0x04 != 0
      - id: will_topic
        type: mqtt_string
        if: connect_flags & 0x04 != 0
      - id: will_payload
        type: mqtt_string
        if: connect_flags & 0x04 != 0
      - id: username
        type: mqtt_string
        if: connect_flags & 0x80 != 0
      - id: password
        type: mqtt_string
        if: connect_flags & 0x40 != 0
  connack_packet:
    seq:
      - id: ack_flags
        type: u1
      - id: reason_code
        type: u1
      - id: properties
        type: property
  publish_packet:
    seq:
      - id: topic_name
        type: mqtt_string
      - id: packet_id
        type: u2
        if: "_parent.header.flags & 0x06 != 0"
      - id: properties
        type: property
      - id: payload
        size-eos: true
  puback_packet:
    seq:
      - id: packet_id
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: property
  pubrec_packet:
    seq:
      - id: packet_id
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: property
  pubrel_packet:
    seq:
      - id: packet_id
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: property
  pubcomp_packet:
    seq:
      - id: packet_id
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: property
  subscribe_packet:
    seq:
      - id: packet_id
        type: u2
      - id: properties
        type: property
      - id: subscriptions
        type: subscription
        repeat: eos
  suback_packet:
    seq:
      - id: packet_id
        type: u2
      - id: properties
        type: property
      - id: reason_codes
        type: u1
        repeat: eos
  unsubscribe_packet:
    seq:
      - id: packet_id
        type: u2
      - id: properties
        type: property
      - id: topic_filters
        type: mqtt_string
        repeat: eos
  unsuback_packet:
    seq:
      - id: packet_id
        type: u2
      - id: properties
        type: property
      - id: reason_codes
        type: u1
        repeat: eos
  disconnect_packet:
    seq:
      - id: reason_code
        type: u1
      - id: properties
        type: property
  auth_packet:
    seq:
      - id: reason_code
        type: u1
      - id: properties
        type: property
  empty_packet:
    seq: []
  mqtt_string:
    seq:
      - id: len
        type: u2
      - id: data
        type: str
        size: len
        encoding: UTF-8
  property:
    seq:
      - id: identifier
        type: u1
      - id: value
        type: u4 # Simplified, actual type depends on the property identifier
  subscription:
    seq:
      - id: topic_filter
        type: mqtt_string
      - id: options
        type: u1