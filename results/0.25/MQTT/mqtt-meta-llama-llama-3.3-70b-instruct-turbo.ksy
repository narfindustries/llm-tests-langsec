meta:
  id: mqtt
  endian: le

seq:
  - id: fixed_header
    type: fixed_header
  - id: variable_header
    type: variable_header
    if: fixed_header.message_type != 1 and fixed_header.message_type != 8 and fixed_header.message_type != 13
  - id: payload
    type: payload

types:
  fixed_header:
    seq:
      - id: message_type
        type: u4
      - id: dup
        type: u1
      - id: qos
        type: u2
      - id: retain
        type: u1
      - id: remaining_length
        type: varint

  varint:
    seq:
      - id: first_byte
        type: u1
      - id: remaining_bytes
        type: u1
        repeat: until first_byte & 0x80 == 0

  variable_header:
    seq:
      - id: packet_id
        type: u2

  payload:
    seq:
      - id: data
        type: payload_data
        if: fixed_header.message_type == 1
      - id: data
        type: payload_data
        if: fixed_header.message_type == 2
      - id: data
        type: payload_data
        if: fixed_header.message_type == 3
      - id: data
        type: payload_data
        if: fixed_header.message_type == 4
      - id: data
        type: payload_data
        if: fixed_header.message_type == 5
      - id: data
        type: payload_data
        if: fixed_header.message_type == 6
      - id: data
        type: payload_data
        if: fixed_header.message_type == 7
      - id: data
        type: payload_data
        if: fixed_header.message_type == 8
      - id: data
        type: payload_data
        if: fixed_header.message_type == 9
      - id: data
        type: payload_data
        if: fixed_header.message_type == 10
      - id: data
        type: payload_data
        if: fixed_header.message_type == 11
      - id: data
        type: payload_data
        if: fixed_header.message_type == 12
      - id: data
        type: payload_data
        if: fixed_header.message_type == 13
      - id: data
        type: payload_data
        if: fixed_header.message_type == 14

  payload_data:
    seq:
      - id: connect
        type: connect
        if: fixed_header.message_type == 1
      - id: connack
        type: connack
        if: fixed_header.message_type == 2
      - id: publish
        type: publish
        if: fixed_header.message_type == 3
      - id: puback
        type: puback
        if: fixed_header.message_type == 4
      - id: pubrec
        type: pubrec
        if: fixed_header.message_type == 5
      - id: pubrel
        type: pubrel
        if: fixed_header.message_type == 6
      - id: pubcomp
        type: pubcomp
        if: fixed_header.message_type == 7
      - id: subscribe
        type: subscribe
        if: fixed_header.message_type == 8
      - id: suback
        type: suback
        if: fixed_header.message_type == 9
      - id: unsubscribe
        type: unsubscribe
        if: fixed_header.message_type == 10
      - id: unsuback
        type: unsuback
        if: fixed_header.message_type == 11
      - id: pingreq
        type: pingreq
        if: fixed_header.message_type == 12
      - id: pingresp
        type: pingresp
        if: fixed_header.message_type == 13
      - id: disconnect
        type: disconnect
        if: fixed_header.message_type == 14

  connect:
    seq:
      - id: protocol_name
        type: str
        encoding: UTF-8
        if: true
      - id: protocol_level
        type: u1
      - id: connect_flags
        type: connect_flags
      - id: keep_alive
        type: u2
      - id: properties
        type: properties
        if: connect_flags.has_property
      - id: username
        type: str
        encoding: UTF-8
        if: connect_flags.has_username
      - id: password
        type: str
        encoding: UTF-8
        if: connect_flags.has_password
      - id: will_topic
        type: str
        encoding: UTF-8
        if: connect_flags.has_will
      - id: will_message
        type: str
        encoding: UTF-8
        if: connect_flags.has_will

  connack:
    seq:
      - id: session_present
        type: u1
      - id: return_code
        type: u1
      - id: properties
        type: properties
        if: return_code == 0

  publish:
    seq:
      - id: topic_name
        type: str
        encoding: UTF-8
      - id: packet_id
        type: u2
        if: fixed_header.qos > 0
      - id: properties
        type: properties
      - id: payload
        type: str
        encoding: UTF-8
        size: eos

  puback:
    seq:
      - id: packet_id
        type: u2

  pubrec:
    seq:
      - id: packet_id
        type: u2

  pubrel:
    seq:
      - id: packet_id
        type: u2

  pubcomp:
    seq:
      - id: packet_id
        type: u2

  subscribe:
    seq:
      - id: packet_id
        type: u2
      - id: properties
        type: properties
      - id: subscriptions
        type: subscription
        repeat: expr

  subscription:
    seq:
      - id: topic_filter
        type: str
        encoding: UTF-8
      - id: requested_qos
        type: u1
      - id: properties
        type: properties
        if: requested_qos == 2

  suback:
    seq:
      - id: packet_id
        type: u2
      - id: return_codes
        type: u1
        repeat: expr

  unsubscribe:
    seq:
      - id: packet_id
        type: u2
      - id: properties
        type: properties
      - id: topic_filters
        type: str
        encoding: UTF-8
        repeat: expr

  unsuback:
    seq:
      - id: packet_id
        type: u2
      - id: return_codes
        type: u1
        repeat: expr

  pingreq:
    seq: []

  pingresp:
    seq: []

  disconnect:
    seq:
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  connect_flags:
    seq:
      - id: has_username
        type: u1
      - id: has_password
        type: u1
      - id: has_will_retain
        type: u1
      - id: will_qos
        type: u2
      - id: will_flag
        type: u1
      - id: clean_session
        type: u1
      - id: has_property
        type: u1

  properties:
    seq:
      - id: property_length
        type: u2
      - id: properties
        type: property
        repeat: expr

  property:
    seq:
      - id: property_id
        type: u1
      - id: property_value
        type: str
        encoding: UTF-8

  str:
    seq:
      - id: length
        type: u2
      - id: data
        type: str
        encoding: UTF-8
        size: length