meta:
  id: mqtt
  endian: be

types:
  fixed_header:
    seq:
      - id: packet_type
        type: u1
      - id: flags
        type: flags
      - id: remaining_length
        type: varint

  flags:
    bits: 8
    seq:
      - id: dup
        type: u1
        pos: 3
      - id: qos
        type: u2
        pos: 1
      - id: retain
        type: u1
        pos: 0
      - id: will
        type: u1
        pos: 2
      - id: will_qos
        type: u1
        pos: 4
      - id: will_retain
        type: u1
        pos: 5
      - id: password
        type: u1
        pos: 6
      - id: clean_session
        type: u1
        pos: 7

  varint:
    process: xor
    xor:
      - 7

  u1:
    type: uint8
    display-type: int

  u2:
    type: uint16
    display-type: int

  u4:
    type: uint32
    display-type: int

  connect_flags:
    bits: 8
    seq:
      - id: clean_session
        type: u1
        pos: 1
      - id: will_flag
        type: u1
        pos: 2
      - id: will_qos
        type: u2
        pos: 3
      - id: will_retain
        type: u1
        pos: 5
      - id: password_flag
        type: u1
        pos: 6
      - id: username_flag
        type: u1
        pos: 7

  connect:
    seq:
      - id: protocol_name
        type: strz
      - id: protocol_level
        type: u1
      - id: connect_flags
        type: connect_flags
      - id: keep_alive
        type: u2
      - id: client_id
        type: strz
      - id: will_topic
        type: strz
        if: connect_flags.will_flag == 1
      - id: will_message
        type: strz
        if: connect_flags.will_flag == 1
      - id: username
        type: strz
        if: connect_flags.username_flag == 1
      - id: password
        type: strz
        if: connect_flags.password_flag == 1

  publish:
    seq:
      - id: topic_name
        type: strz
      - id: packet_id
        type: u2
        if: parent.fixed_header.flags.qos > 0
      - id: payload
        type: bytes

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
      - id: subscriptions
        type: subscription
        repeat: expr
        repeat-expr: packet_id + 2

  subscription:
    seq:
      - id: topic_filter
        type: strz
      - id: qos
        type: u1

  suback:
    seq:
      - id: packet_id
        type: u2
      - id: return_codes
        type: u1
        repeat: expr
        repeat-expr: packet_id + 2

  unsubscribe:
    seq:
      - id: packet_id
        type: u2
      - id: topic_filters
        type: strz
        repeat: expr
        repeat-expr: packet_id + 2

  auth:
    seq:
      - id: reason_code
        type: u1
      - id: authentication_method
        type: strz
      - id: authentication_data
        type: bytes

  connack_props:
    seq:
      - id: session_present
        type: u1
      - id: reason_code
        type: u1
      - id: user_properties
        type: user_property
        repeat: expr
        repeat-expr: until-eof

  connect_props:
    seq:
      - id: session_expiry_interval
        type: u4
      - id: receive_maximum
        type: u2
      - id: maximum_qos
        type: u1
      - id: retain_available
        type: u1
      - id: maximum_packet_size
        type: u4
      - id: topic_alias_maximum
        type: u2
      - id: request_response_information
        type: u1
      - id: request_problem_information
        type: u1
      - id: user_properties
        type: user_property
        repeat: expr
        repeat-expr: until-eof
      - id: authentication_method
        type: strz
      - id: authentication_data
        type: bytes

  publish_props:
    seq:
      - id: payload_format_indicator
        type: u1
      - id: message_expiry_interval
        type: u4
      - id: topic_alias
        type: u2
      - id: response_topic
        type: strz
      - id: correlation_data
        type: bytes
      - id: user_properties
        type: user_property
        repeat: expr
        repeat-expr: until-eof
      - id: subscription_identifier
        type: u2

  subscribe_props:
    seq:
      - id: subscription_identifier
        type: u2
      - id: user_properties
        type: user_property
        repeat: expr
        repeat-expr: until-eof

  suback_props:
    seq:
      - id: reason_code
        type: u1
      - id: user_properties
        type: user_property
        repeat: expr
        repeat-expr: until-eof

  unsubscribe_props:
    seq:
      - id: user_properties
        type: user_property
        repeat: expr
        repeat-expr: until-eof

  auth_props:
    seq:
      - id: reason_code
        type: u1
      - id: authentication_method
        type: strz
      - id: authentication_data
        type: bytes
      - id: user_properties
        type: user_property
        repeat: expr
        repeat-expr: until-eof

  user_property:
    seq:
      - id: key
        type: strz
      - id: value
        type: strz

strz:
  type: string
  encoding: utf8
  term: 0

bytes:
  type: bytes

seq:
  - id: fixed_header
    type: fixed_header
  - id: variable_header
    type: variable_header
  - id: payload_data
    type:
      switch-on: fixed_header.packet_type
      cases:
        1:
          type: connect
        3:
          type: publish
        4:
          type: puback
        5:
          type: pubrec
        6:
          type: pubrel
        7:
          type: pubcomp
        8:
          type: subscribe
        9:
          type: suback
        10:
          type: unsubscribe
        11:
          type: auth
        12:
          type: auth

variable_header:
  switch-on: fixed_header.packet_type
  cases:
    1:
      type: connect_props
    2:
      type: connack_props
    3:
      type: publish_props
    8:
      type: subscribe_props
    9:
      type: suback_props
    10:
      type: unsubscribe_props
    11:
      type: auth_props