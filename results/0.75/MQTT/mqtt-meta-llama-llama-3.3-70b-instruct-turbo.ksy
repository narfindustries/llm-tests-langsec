seq:
  - id: fixed_header
    type: fixed_header
  - id: variable_header
    type: variable_header
  - id: payload
    type: payload

types:
  fixed_header:
    seq:
      - id: control_packet_type
        type: u1
      - id: flags
        type: u1
    instances:
      control_packet_type:
        map:
          0: reserved
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
          15: reserved
      flags:
        switch-on: control_packet_type
        cases:
          connect:
            type: connect_flags
          connack:
            type: connack_flags
          publish:
            type: publish_flags
          puback: {}
          pubrec: {}
          pubrel: {}
          pubcomp: {}
          subscribe:
            type: subscribe_flags
          suback: {}
          unsubscribe:
            type: unsubscribe_flags
          unsuback: {}
          pingreq: {}
          pingresp: {}
          disconnect:
            type: disconnect_flags

  variable_header:
    seq:
      - id: protocol_name
        type: strz
        size: 4
      - id: protocol_version
        type: u1
      - id: connect_flags
        type: connect_flags
      - id: keep_alive
        type: u2le
      - id: properties
        type: properties
        if: control_packet_type == connect
      - id: packet_identifier
        type: u2le
        if: control_packet_type in [publish, puback, pubrec, pubrel, pubcomp, subscribe, suback, unsubscribe, unsuback]
      - id: topic_name
        type: strz
        if: control_packet_type == publish
      - id: topic_alias
        type: u2le
        if: control_packet_type == publish
    instances:
      protocol_name:
        value: "MQTT"

  payload:
    switch-on: control_packet_type
    cases:
      connect:
        type: connect_payload
      connack:
        type: connack_payload
      publish:
        type: publish_payload
      puback:
        type: puback_payload
      pubrec:
        type: pubrec_payload
      pubrel:
        type: pubrel_payload
      pubcomp:
        type: pubcomp_payload
      subscribe:
        type: subscribe_payload
      suback:
        type: suback_payload
      unsubscribe:
        type: unsubscribe_payload
      unsuback:
        type: unsuback_payload
      pingreq: {}
      pingresp: {}
      disconnect:
        type: disconnect_payload

  connect_flags:
    seq:
      - id: clean_start
        type: u1
        bits: 1
      - id: will_flag
        type: u1
        bits: 1
      - id: will_qos
        type: u1
        bits: 2
      - id: will_retain
        type: u1
        bits: 1
      - id: password_flag
        type: u1
        bits: 1
      - id: username_flag
        type: u1
        bits: 1

  connack_flags:
    seq:
      - id: session_present
        type: u1
        bits: 1

  publish_flags:
    seq:
      - id: dup
        type: u1
        bits: 1
      - id: qos
        type: u1
        bits: 2
      - id: retain
        type: u1
        bits: 1

  subscribe_flags:
    seq:
      - id: qos
        type: u1
        bits: 1
      - id: no_local
        type: u1
        bits: 1
      - id: retain_as_published
        type: u1
        bits: 1
      - id: retain_handling
        type: u1
        bits: 2

  unsubscribe_flags:
    seq:
      - id: user_property
        type: u1
        bits: 1

  disconnect_flags:
    seq:
      - id: reason_code
        type: u1
        bits: 8

  connect_payload:
    seq:
      - id: session_expiry_interval
        type: u4le
      - id: receive_maximum
        type: u2le
      - id: maximum_packet_size
        type: u4le
      - id: topic_alias_maximum
        type: u2le
      - id: request_response_information
        type: u1
      - id: request_problem_information
        type: u1
      - id: user_property
        type: properties
      - id: authentication_method
        type: strz
      - id: authentication_data
        type: bytes

  connack_payload:
    seq:
      - id: session_present
        type: u1
      - id: connect_reason_code
        type: u1
      - id: connect_return_code
        type: u1
      - id: assigned_client_identifier
        type: strz
        encoding: UTF-8
      - id: reason_string
        type: strz
        encoding: UTF-8
      - id: user_property
        type: properties
      - id: server_reference
        type: strz
        encoding: UTF-8

  publish_payload:
    seq:
      - id: payload
        type: bytes

  puback_payload:
    seq:
      - id: reason_code
        type: u1
      - id: reason_string
        type: strz
        encoding: UTF-8

  pubrec_payload:
    seq:
      - id: reason_code
        type: u1
      - id: reason_string
        type: strz
        encoding: UTF-8

  pubrel_payload:
    seq:
      - id: reason_code
        type: u1
      - id: reason_string
        type: strz
        encoding: UTF-8

  pubcomp_payload:
    seq:
      - id: reason_code
        type: u1
      - id: reason_string
        type: strz
        encoding: UTF-8

  subscribe_payload:
    seq:
      - id: subscriptions
        type: subscription
        repeat: expr
      - id: user_property
        type: properties

  subscription:
    seq:
      - id: topic_filter
        type: strz
        encoding: UTF-8
      - id: qos
        type: u1
      - id: no_local
        type: u1
      - id: retain_as_published
        type: u1
      - id: retain_handling
        type: u1

  suback_payload:
    seq:
      - id: return_codes
        type: u1
        repeat: expr

  unsubscribe_payload:
    seq:
      - id: topic_filters
        type: strz
        repeat: expr
        encoding: UTF-8
      - id: user_property
        type: properties

  unsuback_payload:
    seq:
      - id: return_codes
        type: u1
        repeat: expr
      - id: reason_string
        type: strz
        encoding: UTF-8
      - id: user_property
        type: properties

  disconnect_payload:
    seq:
      - id: reason_code
        type: u1
      - id: reason_string
        type: strz
        encoding: UTF-8
      - id: user_property
        type: properties
      - id: server_reference
        type: strz
        encoding: UTF-8

  properties:
    seq:
      - id: property_length
        type: u4le
      - id: properties
        type: property
        repeat: expr

  property:
    seq:
      - id: identifier
        type: u1
      - id: value
        type: bytes

  strz:
    seq:
      - id: length
        type: u2le
      - id: value
        type: byte
        repeat: expr
        encoding: UTF-8

  bytes:
    seq:
      - id: length
        type: u4le
      - id: value
        type: byte
        repeat: expr

  byte:
    type: u1

  u1:
    type: uint1

  u2le:
    type: uint2le

  u4le:
    type: uint4le

  uint1:
    type: int
    bits: 8

  uint2le:
    type: int
    bits: 16
    endianness: le

  uint4le:
    type: int
    bits: 32
    endianness: le