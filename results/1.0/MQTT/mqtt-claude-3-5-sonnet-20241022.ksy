meta:
  id: mqtt
  title: MQTT Protocol v5.0
  file-extension: mqtt
  endian: be

seq:
  - id: fixed_header
    type: fixed_header
  - id: variable_header
    type: variable_header
    size-eos: true

types:
  fixed_header:
    seq:
      - id: packet_type_and_flags
        type: u1
      - id: remaining_length
        type: var_int
    instances:
      packet_type:
        value: (packet_type_and_flags & 0xF0) >> 4
        enum: packet_types
      dup_flag:
        value: (packet_type_and_flags & 0x08) >> 3
      qos_level:
        value: (packet_type_and_flags & 0x06) >> 1
      retain:
        value: packet_type_and_flags & 0x01

  var_int:
    seq:
      - id: byte1
        type: u1
      - id: byte2
        type: u1
        if: (byte1 & 0x80) != 0
      - id: byte3
        type: u1
        if: (byte2 & 0x80) != 0
      - id: byte4
        type: u1
        if: (byte3 & 0x80) != 0
    instances:
      value:
        value: >-
          byte1 & 0x7f +
          ((byte2 & 0x7f) << 7) * (byte1 & 0x80 != 0 ? 1 : 0) +
          ((byte3 & 0x7f) << 14) * (byte2 & 0x80 != 0 ? 1 : 0) +
          ((byte4 & 0x7f) << 21) * (byte3 & 0x80 != 0 ? 1 : 0)

  variable_header:
    seq:
      - id: protocol_name
        type: mqtt_string
        if: _parent.fixed_header.packet_type == packet_types::connect
      - id: protocol_version
        type: u1
        if: _parent.fixed_header.packet_type == packet_types::connect
      - id: connect_flags
        type: u1
        if: _parent.fixed_header.packet_type == packet_types::connect
      - id: keep_alive
        type: u2
        if: _parent.fixed_header.packet_type == packet_types::connect
      - id: properties_length
        type: var_int
      - id: properties
        type: property
        repeat: expr
        repeat-expr: properties_length.value
      - id: payload
        type:
          switch-on: _parent.fixed_header.packet_type
          cases:
            'packet_types::connect': connect_payload
            'packet_types::publish': publish_payload
            'packet_types::subscribe': subscribe_payload
            'packet_types::unsubscribe': unsubscribe_payload

  mqtt_string:
    seq:
      - id: len_data
        type: u2
      - id: value
        type: str
        size: len_data
        encoding: UTF-8

  property:
    seq:
      - id: identifier
        type: u1
        enum: property_ids
      - id: value
        type:
          switch-on: identifier
          cases:
            'property_ids::payload_format_indicator': u1
            'property_ids::message_expiry_interval': u4
            'property_ids::content_type': mqtt_string
            'property_ids::response_topic': mqtt_string
            'property_ids::correlation_data': binary_data
            'property_ids::subscription_identifier': var_int
            'property_ids::session_expiry_interval': u4
            'property_ids::assigned_client_identifier': mqtt_string
            'property_ids::server_keep_alive': u2
            'property_ids::authentication_method': mqtt_string
            'property_ids::authentication_data': binary_data
            'property_ids::request_problem_information': u1
            'property_ids::will_delay_interval': u4
            'property_ids::request_response_information': u1
            'property_ids::response_information': mqtt_string
            'property_ids::server_reference': mqtt_string
            'property_ids::reason_string': mqtt_string
            'property_ids::receive_maximum': u2
            'property_ids::topic_alias_maximum': u2
            'property_ids::topic_alias': u2
            'property_ids::maximum_qos': u1
            'property_ids::retain_available': u1
            'property_ids::user_property': user_property
            'property_ids::maximum_packet_size': u4
            'property_ids::wildcard_subscription_available': u1
            'property_ids::subscription_identifier_available': u1
            'property_ids::shared_subscription_available': u1

  binary_data:
    seq:
      - id: len_data
        type: u2
      - id: data
        size: len_data

  user_property:
    seq:
      - id: name
        type: mqtt_string
      - id: value
        type: mqtt_string

  connect_payload:
    seq:
      - id: client_id
        type: mqtt_string
      - id: will_properties
        type: property
        repeat: expr
        repeat-expr: _parent.properties_length.value
        if: _parent._parent.variable_header.connect_flags & 0x04 != 0
      - id: will_topic
        type: mqtt_string
        if: _parent._parent.variable_header.connect_flags & 0x04 != 0
      - id: will_payload
        type: binary_data
        if: _parent._parent.variable_header.connect_flags & 0x04 != 0
      - id: username
        type: mqtt_string
        if: _parent._parent.variable_header.connect_flags & 0x80 != 0
      - id: password
        type: mqtt_string
        if: _parent._parent.variable_header.connect_flags & 0x40 != 0

  publish_payload:
    seq:
      - id: data
        size-eos: true

  subscribe_payload:
    seq:
      - id: subscription_identifier
        type: var_int
      - id: topics
        type: subscribe_topic
        repeat: eos

  subscribe_topic:
    seq:
      - id: topic_filter
        type: mqtt_string
      - id: subscription_options
        type: u1

  unsubscribe_payload:
    seq:
      - id: topics
        type: mqtt_string
        repeat: eos

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

  property_ids:
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
    23: request_problem_information
    24: will_delay_interval
    25: request_response_information
    26: response_information
    28: server_reference
    31: reason_string
    33: receive_maximum
    34: topic_alias_maximum
    35: topic_alias
    36: maximum_qos
    37: retain_available
    38: user_property
    39: maximum_packet_size
    40: wildcard_subscription_available
    41: subscription_identifier_available
    42: shared_subscription_available

  reason_codes:
    0x00: success
    0x01: normal_disconnection
    0x02: granted_qos0
    0x03: granted_qos1
    0x04: granted_qos2
    0x80: unspecified_error
    0x81: malformed_packet
    0x82: protocol_error
    0x83: implementation_specific_error
    0x84: unsupported_protocol_version
    0x85: client_identifier_not_valid
    0x86: bad_username_or_password
    0x87: not_authorized
    0x88: server_unavailable
    0x89: server_busy
    0x8a: banned
    0x8b: server_shutting_down
    0x8c: bad_authentication_method
    0x8d: keep_alive_timeout
    0x8e: session_taken_over
    0x8f: topic_filter_invalid
    0x90: topic_name_invalid
    0x91: packet_identifier_in_use
    0x92: packet_identifier_not_found
    0x93: receive_maximum_exceeded
    0x94: topic_alias_invalid
    0x95: packet_too_large
    0x96: message_rate_too_high
    0x97: quota_exceeded
    0x98: administrative_action
    0x99: payload_format_invalid
    0x9a: retain_not_supported
    0x9b: qos_not_supported
    0x9c: use_another_server
    0x9d: server_moved
    0x9e: shared_subscriptions_not_supported
    0x9f: connection_rate_exceeded
    0xa0: maximum_connect_time
    0xa1: subscription_identifiers_not_supported
    0xa2: wildcard_subscriptions_not_supported