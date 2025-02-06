meta:
  id: mqtt
  title: MQTT Protocol v5.0
  file-extension: mqtt
  endian: be

seq:
  - id: fixed_header
    type: fixed_header
  - id: variable_header_and_payload
    type: 
      switch-on: fixed_header.packet_type
      cases:
        0x01: connect
        0x02: connack
        0x03: publish
        0x04: puback
        0x05: pubrec
        0x06: pubrel
        0x07: pubcomp
        0x08: subscribe
        0x09: suback
        0x0a: unsubscribe
        0x0b: unsuback
        0x0c: pingreq
        0x0d: pingresp
        0x0e: disconnect
        0x0f: auth
    size: fixed_header.remaining_length.value

types:
  fixed_header:
    seq:
      - id: packet_type
        type: b4
      - id: flags
        type: b4
      - id: remaining_length
        type: var_int

  var_int:
    seq:
      - id: byte1
        type: u1
      - id: has_more1
        type: b1
        enum: has_more
      - id: byte2
        type: u1
        if: has_more1 == has_more::yes
      - id: has_more2
        type: b1
        if: has_more1 == has_more::yes
        enum: has_more
      - id: byte3
        type: u1
        if: has_more1 == has_more::yes and has_more2 == has_more::yes
      - id: has_more3
        type: b1
        if: has_more1 == has_more::yes and has_more2 == has_more::yes
        enum: has_more
      - id: byte4
        type: u1
        if: has_more1 == has_more::yes and has_more2 == has_more::yes and has_more3 == has_more::yes
    instances:
      value:
        value: >-
          (byte1 & 0x7f) +
          ((byte2 & 0x7f) << 7 if has_more1 == has_more::yes else 0) +
          ((byte3 & 0x7f) << 14 if has_more1 == has_more::yes and has_more2 == has_more::yes else 0) +
          ((byte4 & 0x7f) << 21 if has_more1 == has_more::yes and has_more2 == has_more::yes and has_more3 == has_more::yes else 0)
    enums:
      has_more:
        0: no
        1: yes

  string:
    seq:
      - id: len
        type: u2
      - id: value
        type: str
        size: len
        encoding: UTF-8

  binary_data:
    seq:
      - id: len
        type: u2
      - id: data
        size: len

  properties:
    seq:
      - id: length
        type: var_int
      - id: properties
        type: property
        repeat: until
        repeat-until: _io.pos - _parent.length.value >= length.value
        if: length.value > 0

  property:
    seq:
      - id: identifier
        type: u1
      - id: value
        type:
          switch-on: identifier
          cases:
            0x01: u1  # payload_format_indicator
            0x02: u4  # message_expiry_interval
            0x03: string  # content_type
            0x08: string  # response_topic
            0x09: binary_data  # correlation_data
            0x0b: var_int  # subscription_identifier
            0x11: u4  # session_expiry_interval
            0x12: string  # assigned_client_identifier
            0x13: u2  # server_keep_alive
            0x15: string  # authentication_method
            0x16: binary_data  # authentication_data
            0x17: u1  # request_problem_information
            0x18: u4  # will_delay_interval
            0x19: u1  # request_response_information
            0x1a: string  # response_information
            0x1c: string  # server_reference
            0x1f: string  # reason_string
            0x21: u2  # receive_maximum
            0x22: u2  # topic_alias_maximum
            0x23: u2  # topic_alias
            0x24: u1  # maximum_qos
            0x25: u1  # retain_available
            0x26: user_property  # user_property
            0x27: u4  # maximum_packet_size
            0x28: u1  # wildcard_subscription_available
            0x29: u1  # subscription_identifier_available
            0x2a: u1  # shared_subscription_available

  user_property:
    seq:
      - id: key
        type: string
      - id: value
        type: string

  connect:
    seq:
      - id: protocol_name
        type: string
      - id: protocol_version
        type: u1
      - id: connect_flags
        type: u1
      - id: keep_alive
        type: u2
      - id: properties
        type: properties
      - id: client_id
        type: string
      - id: will_properties
        type: properties
        if: (connect_flags & 0x04) != 0
      - id: will_topic
        type: string
        if: (connect_flags & 0x04) != 0
      - id: will_payload
        type: binary_data
        if: (connect_flags & 0x04) != 0
      - id: username
        type: string
        if: (connect_flags & 0x80) != 0
      - id: password
        type: binary_data
        if: (connect_flags & 0x40) != 0

  connack:
    seq:
      - id: acknowledge_flags
        type: u1
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  publish:
    seq:
      - id: topic_name
        type: string
      - id: packet_id
        type: u2
        if: (_parent.fixed_header.flags & 0x06) != 0
      - id: properties
        type: properties
      - id: payload
        size-eos: true

  puback:
    seq:
      - id: packet_id
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  pubrec:
    seq:
      - id: packet_id
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  pubrel:
    seq:
      - id: packet_id
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  pubcomp:
    seq:
      - id: packet_id
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  subscribe:
    seq:
      - id: packet_id
        type: u2
      - id: properties
        type: properties
      - id: subscriptions
        type: subscription
        repeat: eos

  subscription:
    seq:
      - id: topic_filter
        type: string
      - id: subscription_options
        type: u1

  suback:
    seq:
      - id: packet_id
        type: u2
      - id: properties
        type: properties
      - id: reason_codes
        type: u1
        repeat: eos

  unsubscribe:
    seq:
      - id: packet_id
        type: u2
      - id: properties
        type: properties
      - id: topic_filters
        type: string
        repeat: eos

  unsuback:
    seq:
      - id: packet_id
        type: u2
      - id: properties
        type: properties
      - id: reason_codes
        type: u1
        repeat: eos

  disconnect:
    seq:
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  auth:
    seq:
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  pingreq: {}
  pingresp: {}