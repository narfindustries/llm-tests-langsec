meta:
  id: mqtt_v5
  endian: be
  title: MQTT Version 5.0 Protocol
  license: MIT

seq:
  - id: packet_type
    type: b4
    enum: packet_types
  - id: flags
    type: b4
  - id: remaining_length
    type: vlq_base128_le

types:
  connect_packet:
    seq:
      - id: protocol_name
        type: strz
        encoding: ascii
        size: 4
      - id: protocol_version
        type: u1
        valid: 5
      - id: connect_flags
        type: connect_flags
      - id: keep_alive
        type: u2
      - id: properties
        type: properties_list
      - id: client_identifier
        type: prefixed_string
      - id: will_properties
        type: properties_list
        if: connect_flags.will_flag
      - id: will_topic
        type: prefixed_string
        if: connect_flags.will_flag
      - id: will_payload
        type: prefixed_byte_array(will_topic.length)
        if: connect_flags.will_flag
      - id: username
        type: prefixed_string
        if: connect_flags.username_flag
      - id: password
        type: prefixed_string
        if: connect_flags.password_flag

  connect_flags:
    seq:
      - id: reserved
        type: b1
      - id: clean_start
        type: b1
      - id: will_flag
        type: b1
      - id: will_qos
        type: b2
      - id: will_retain
        type: b1
      - id: password_flag
        type: b1
      - id: username_flag
        type: b1

  properties_list:
    seq:
      - id: length
        type: vlq_base128_le
      - id: properties
        type: property
        repeat: expr
        repeat-expr: length.value

  property:
    seq:
      - id: identifier
        type: u1
        enum: property_identifiers
      - id: value
        type:
          switch-on: identifier
          cases:
            'property_identifiers::payload_format_indicator': u1
            'property_identifiers::message_expiry_interval': u4
            'property_identifiers::content_type': prefixed_string
            'property_identifiers::response_topic': prefixed_string
            'property_identifiers::correlation_data': prefixed_byte_array(0)
            'property_identifiers::subscription_identifier': vlq_base128_le
            'property_identifiers::session_expiry_interval': u4
            'property_identifiers::assigned_client_identifier': prefixed_string
            'property_identifiers::server_keep_alive': u2
            'property_identifiers::authentication_method': prefixed_string
            'property_identifiers::authentication_data': prefixed_byte_array(0)

  prefixed_string:
    seq:
      - id: length
        type: u2
      - id: value
        type: str
        size: length
        encoding: utf-8

  prefixed_byte_array:
    params:
      - id: num_value
        type: u2
    seq:
      - id: value
        type: u1
        repeat: expr
        repeat-expr: num_value

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
          + (groups.size >= 2 ? groups[1].value << 7 : 0)
          + (groups.size >= 3 ? groups[2].value << 14 : 0)
          + (groups.size >= 4 ? groups[3].value << 21 : 0)

  vlq_group:
    seq:
      - id: has_next
        type: b1
      - id: value
        type: b7

enums:
  packet_types:
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
    15: auth

  property_identifiers:
    1: payload_format_indicator
    2: message_expiry_interval
    3: content_type
    8: response_topic
    9: correlation_data
    11: subscription_identifier
    17: session_expiry_interval
    18: assigned_client_identifier
    19: server_keep_alive
    21: authentication_method
    22: authentication_data