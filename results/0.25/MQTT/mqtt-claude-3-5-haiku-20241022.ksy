meta:
  id: mqtt
  title: MQTT Protocol
  file-extension: mqtt
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
  - id: payload
    type:
      switch-on: fixed_header.packet_type
      cases:
        1: connect_payload
        3: publish_payload
        8: subscribe_payload
        10: unsubscribe_payload

types:
  fixed_header:
    seq:
      - id: packet_type
        type: b4
      - id: flags
        type: b4
      - id: remaining_length
        type: vlq_base128_le

  connect_variable_header:
    seq:
      - id: protocol_name
        type: str
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
        value: will_flag.to_i == 1

  connect_payload:
    seq:
      - id: client_id
        type: prefixed_str
      - id: will_properties
        type: properties
        if: _parent.variable_header.connect_flags.has_will
      - id: will_topic
        type: prefixed_str
        if: _parent.variable_header.connect_flags.has_will
      - id: will_message
        type: prefixed_str
        if: _parent.variable_header.connect_flags.has_will
      - id: username
        type: prefixed_str
        if: _parent.variable_header.connect_flags.username_flag.to_i == 1
      - id: password
        type: prefixed_str
        if: _parent.variable_header.connect_flags.password_flag.to_i == 1

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
        type: prefixed_str
      - id: packet_identifier
        type: u2
        if: _parent.fixed_header.flags & 0b0110 > 0
      - id: properties
        type: properties

  publish_payload:
    seq:
      - id: payload
        type: byte_array
        size-eos: true

  byte_array:
    seq:
      - id: data
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
        type: prefixed_str
      - id: subscription_options
        type: u1

  suback_variable_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties
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
        type: prefixed_str
        repeat: eos

  unsuback_variable_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties
      - id: reason_codes
        type: u1
        repeat: eos

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

  properties:
    seq:
      - id: property_length
        type: vlq_base128_le
      - id: property_list
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
            17: prefixed_str  # Response Topic
            18: byte_array  # Correlation Data
            21: u1  # Payload Format Indicator
            23: u4  # Message Expiry Interval
            25: u2  # Topic Alias
            31: prefixed_str  # Reason String
            33: user_property  # User Properties
            35: u4  # Subscription Identifier
            36: u1  # Content Type

  user_property:
    seq:
      - id: key
        type: prefixed_str
      - id: value
        type: prefixed_str

  prefixed_str:
    seq:
      - id: length
        type: u2
      - id: value
        type: str
        encoding: UTF-8
        size: length

  vlq_base128_le:
    seq:
      - id: groups
        type: vlq_group
        repeat: until
        repeat-until: not _.has_next
    instances:
      value:
        value: >-
          groups[0].value
          + (groups.size >= 2 ? groups[1].value * 0x80 : 0)
          + (groups.size >= 3 ? groups[2].value * 0x80 * 0x80 : 0)
          + (groups.size >= 4 ? groups[3].value * 0x80 * 0x80 * 0x80 : 0)

  vlq_group:
    seq:
      - id: has_next
        type: b1
      - id: value
        type: b7