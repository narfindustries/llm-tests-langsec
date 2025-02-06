meta:
  id: mqtt_v5
  title: MQTT Version 5.0 Protocol
  endian: be

seq:
  - id: fixed_header
    type: fixed_header
  - id: variable_header
    type:
      switch-on: fixed_header.packet_type
      cases:
        1: connect_variable_header
        2: connack_variable_header
        3: publish_variable_header
        4: puback_variable_header
        5: pubrec_variable_header
        6: pubrel_variable_header
        7: pubcomp_variable_header
        8: subscribe_variable_header
        9: suback_variable_header
        10: unsubscribe_variable_header
        11: unsuback_variable_header
        12: pingreq_variable_header
        13: pingresp_variable_header
        14: disconnect_variable_header
        15: auth_variable_header
  - id: payload
    type:
      switch-on: fixed_header.packet_type
      cases:
        1: connect_payload
        3: publish_payload
        8: subscribe_payload
        9: suback_payload
        10: unsubscribe_payload
        11: unsuback_payload

types:
  fixed_header:
    seq:
      - id: packet_type
        type: b4
      - id: flags
        type: b4
      - id: remaining_length
        type: varint

  varint:
    seq:
      - id: groups
        type: varint_group
        repeat: until
        repeat-until: not _.has_next
    instances:
      value:
        value: groups[0].value

  varint_group:
    seq:
      - id: value
        type: b7
      - id: has_next
        type: b1

  connect_variable_header:
    seq:
      - id: protocol_name
        type: str
        size: 4
        encoding: ascii
      - id: protocol_version
        type: u1
      - id: connect_flags
        type: connect_flags
      - id: keep_alive
        type: u2
      - id: properties
        type: properties

  connect_flags:
    seq:
      - id: username_flag
        type: b1
      - id: password_flag
        type: b1
      - id: will_retain
        type: b1
      - id: will_qos
        type: b2
      - id: will_flag
        type: b1
      - id: clean_start
        type: b1
      - id: reserved
        type: b1
    instances:
      has_will:
        value: will_flag == 1
      has_username:
        value: username_flag == 1
      has_password:
        value: password_flag == 1

  connect_payload:
    seq:
      - id: client_id
        type: prefixed_string
      - id: will_properties
        type: properties
        if: _parent.variable_header.connect_flags.has_will
      - id: will_topic
        type: prefixed_string
        if: _parent.variable_header.connect_flags.has_will
      - id: will_payload
        type: prefixed_bytes
        if: _parent.variable_header.connect_flags.has_will
      - id: username
        type: prefixed_string
        if: _parent.variable_header.connect_flags.has_username
      - id: password
        type: prefixed_string
        if: _parent.variable_header.connect_flags.has_password

  connack_variable_header:
    seq:
      - id: connect_acknowledge_flags
        type: u1
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  publish_variable_header:
    seq:
      - id: topic_name
        type: prefixed_string
      - id: packet_identifier
        type: u2
        if: _parent.fixed_header.flags & 0b0110 > 0
      - id: properties
        type: properties

  publish_payload:
    seq:
      - id: payload
        type: raw_bytes
        size-eos: true

  raw_bytes:
    seq:
      - id: data
        type: u1
        repeat: eos

  subscribe_variable_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties

  subscribe_payload:
    seq:
      - id: subscriptions
        type: subscription
        repeat: eos

  subscription:
    seq:
      - id: topic_filter
        type: prefixed_string
      - id: subscription_options
        type: u1

  suback_variable_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties

  suback_payload:
    seq:
      - id: reason_codes
        type: u1
        repeat: eos

  unsubscribe_variable_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties

  unsubscribe_payload:
    seq:
      - id: topic_filters
        type: prefixed_string
        repeat: eos

  unsuback_variable_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties

  unsuback_payload:
    seq:
      - id: reason_codes
        type: u1
        repeat: eos

  puback_variable_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  pubrec_variable_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  pubrel_variable_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  pubcomp_variable_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  pingreq_variable_header:
    seq: []

  pingresp_variable_header:
    seq: []

  disconnect_variable_header:
    seq:
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  auth_variable_header:
    seq:
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  properties:
    seq:
      - id: property_length
        type: varint
      - id: properties_list
        type: property
        repeat: expr
        repeat-expr: property_length.value

  property:
    seq:
      - id: identifier
        type: u1
      - id: value
        type:
          switch-on: identifier
          cases:
            1: u4  # Session Expiry Interval
            2: u2  # Receive Maximum
            3: u4  # Maximum Packet Size
            4: u2  # Topic Alias Maximum
            5: u1  # Request Response Information
            6: u1  # Request Problem Information
            17: prefixed_string  # Response Topic
            18: prefixed_bytes  # Correlation Data
            21: u1  # Payload Format Indicator
            33: u4  # Message Expiry Interval
            35: u2  # Topic Alias
            38: user_properties  # User Properties

  user_properties:
    seq:
      - id: key
        type: prefixed_string
      - id: value
        type: prefixed_string

  prefixed_string:
    seq:
      - id: len_value
        type: u2
      - id: value
        type: str
        size: len_value
        encoding: utf-8

  prefixed_bytes:
    seq:
      - id: len_value
        type: u2
      - id: value
        type: raw_bytes
        size: len_value