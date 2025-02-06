types:
  mqtt_packet:
    seq:
      - id: packet_type
        type: u1
      - id: remaining_length
        type: vlq
      - id: body
        type: switch
        on: packet_type
        cases:
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
          default: unknown_packet

  connect:
    seq:
      - id: protocol_name
        type: str
        len: 4
        encoding: ASCII
      - id: protocol_version
        type: u1
      - id: connect_flags
        type: u2
      - id: keep_alive_interval
        type: u2be
      - id: client_identifier
        type: str
        encoding: UTF-8
      - id: will_properties
        type: mqtt_properties
      - id: will_topic
        type: str
        encoding: UTF-8
        if: connect_flags & 0x04
      - id: will_message
        type: bytes
        if: connect_flags & 0x04
      - id: username
        type: str
        encoding: UTF-8
        if: connect_flags & 0x80
      - id: password
        type: bytes
        if: connect_flags & 0x80
      - id: properties
        type: mqtt_properties

  connack:
    seq:
      - id: session_present
        type: u1
      - id: connect_return_code
        type: u1
      - id: properties
        type: mqtt_properties

  publish:
    seq:
      - id: topic_name
        type: str
        encoding: UTF-8
      - id: packet_identifier
        type: u2be
        if: qos > 0
      - id: publish_flags
        type: u1
      - id: payload
        type: bytes

  puback:
    seq:
      - id: packet_identifier
        type: u2be
      - id: reason_code
        type: u1
      - id: properties
        type: mqtt_properties

  pubrec:
    seq:
      - id: packet_identifier
        type: u2be
      - id: reason_code
        type: u1
      - id: properties
        type: mqtt_properties

  pubrel:
    seq:
      - id: packet_identifier
        type: u2be
      - id: reason_code
        type: u1
      - id: properties
        type: mqtt_properties

  pubcomp:
    seq:
      - id: packet_identifier
        type: u2be
      - id: reason_code
        type: u1
      - id: properties
        type: mqtt_properties

  subscribe:
    seq:
      - id: packet_identifier
        type: u2be
      - id: topic_filters
        type: topic_filter
        repeat: true
      - id: properties
        type: mqtt_properties

  topic_filter:
    seq:
      - id: topic_filter
        type: str
        encoding: UTF-8
      - id: qos
        type: u1

  suback:
    seq:
      - id: packet_identifier
        type: u2be
      - id: return_codes
        type: u1
        repeat: true
      - id: properties
        type: mqtt_properties

  unsubscribe:
    seq:
      - id: packet_identifier
        type: u2be
      - id: topic_filters
        type: str
        encoding: UTF-8
        repeat: true
      - id: properties
        type: mqtt_properties

  unsuback:
    seq:
      - id: packet_identifier
        type: u2be
      - id: reason_codes
        type: u1
        repeat: true
      - id: properties
        type: mqtt_properties

  pingreq:
    seq:
      - id: properties
        type: mqtt_properties

  pingresp:
    seq:
      - id: properties
        type: mqtt_properties

  disconnect:
    seq:
      - id: reason_code
        type: u1
      - id: properties
        type: mqtt_properties

  mqtt_properties:
    seq:
      - id: property_length
        type: vlq
      - id: properties
        type: property
        repeat: expr
        repeat-expr: property_length

  property:
    seq:
      - id: property_id
        type: u2be
      - id: property_value
        type: switch
        on: property_id
        cases:
          # Add all property ID cases here.  This is highly incomplete and needs to be filled from the specification
          default: bytes


  unknown_packet:
    seq:
      - id: remaining_bytes
        type: bytes
