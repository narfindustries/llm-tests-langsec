meta:
  id: mqtt
  title: MQTT Protocol
  file-extension: mqtt
  endian: be

seq:
  - id: fixed_header
    type: fixed_header
  - id: variable_header
    type: variable_header
    size: fixed_header.remaining_length

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
      flags:
        value: packet_type_and_flags & 0x0F
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
      - id: has_more1
        type: u1
        size: 0
        if: (byte1 & 0x80) != 0
      - id: byte2
        type: u1
        if: has_more1
      - id: has_more2
        type: u1
        size: 0
        if: has_more1 and (byte2 & 0x80) != 0
      - id: byte3
        type: u1
        if: has_more2
      - id: has_more3
        type: u1
        size: 0
        if: has_more2 and (byte3 & 0x80) != 0
      - id: byte4
        type: u1
        if: has_more3
    instances:
      value:
        value: >-
          (byte1 & 0x7F) +
          (has_more1 ? ((byte2 & 0x7F) << 7) : 0) +
          (has_more2 ? ((byte3 & 0x7F) << 14) : 0) +
          (has_more3 ? ((byte4 & 0x7F) << 21) : 0)

  variable_header:
    seq:
      - id: connect
        type: connect_packet
        if: _parent.fixed_header.packet_type == packet_types::connect
      - id: connack
        type: connack_packet
        if: _parent.fixed_header.packet_type == packet_types::connack
      - id: publish
        type: publish_packet
        if: _parent.fixed_header.packet_type == packet_types::publish
      - id: puback
        type: puback_packet
        if: _parent.fixed_header.packet_type == packet_types::puback
      - id: subscribe
        type: subscribe_packet
        if: _parent.fixed_header.packet_type == packet_types::subscribe
      - id: suback
        type: suback_packet
        if: _parent.fixed_header.packet_type == packet_types::suback
      - id: unsubscribe
        type: unsubscribe_packet
        if: _parent.fixed_header.packet_type == packet_types::unsubscribe
      - id: unsuback
        type: unsuback_packet
        if: _parent.fixed_header.packet_type == packet_types::unsuback
      - id: disconnect
        type: disconnect_packet
        if: _parent.fixed_header.packet_type == packet_types::disconnect
      - id: auth
        type: auth_packet
        if: _parent.fixed_header.packet_type == packet_types::auth

  mqtt_string:
    seq:
      - id: len
        type: u2
      - id: value
        type: str
        size: len
        encoding: UTF-8

  properties:
    seq:
      - id: length
        type: var_int
      - id: properties
        type: property
        repeat: eos

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
      - id: len
        type: u2
      - id: data
        size: len

  user_property:
    seq:
      - id: key
        type: mqtt_string
      - id: value
        type: mqtt_string

  connect_packet:
    seq:
      - id: protocol_name
        type: mqtt_string
      - id: protocol_version
        type: u1
      - id: connect_flags
        type: u1
      - id: keep_alive
        type: u2
      - id: properties
        type: properties
      - id: client_id
        type: mqtt_string
      - id: will_properties
        type: properties
        if: (connect_flags & 0x04) != 0
      - id: will_topic
        type: mqtt_string
        if: (connect_flags & 0x04) != 0
      - id: will_payload
        type: binary_data
        if: (connect_flags & 0x04) != 0
      - id: username
        type: mqtt_string
        if: (connect_flags & 0x80) != 0
      - id: password
        type: mqtt_string
        if: (connect_flags & 0x40) != 0

  connack_packet:
    seq:
      - id: acknowledge_flags
        type: u1
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  publish_packet:
    seq:
      - id: topic_name
        type: mqtt_string
      - id: packet_identifier
        type: u2
        if: _parent._parent.fixed_header.qos_level > 0
      - id: properties
        type: properties
      - id: payload
        size-eos: true

  puback_packet:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  subscribe_packet:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties
      - id: subscriptions
        type: subscription
        repeat: eos

  subscription:
    seq:
      - id: topic_filter
        type: mqtt_string
      - id: subscription_options
        type: u1

  suback_packet:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties
      - id: reason_codes
        type: u1
        repeat: eos

  unsubscribe_packet:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties
      - id: topic_filters
        type: mqtt_string
        repeat: eos

  unsuback_packet:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties
      - id: reason_codes
        type: u1
        repeat: eos

  disconnect_packet:
    seq:
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  auth_packet:
    seq:
      - id: reason_code
        type: u1
      - id: properties
        type: properties

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
    0x01: payload_format_indicator
    0x02: message_expiry_interval
    0x03: content_type
    0x08: response_topic
    0x09: correlation_data
    0x0B: subscription_identifier
    0x11: session_expiry_interval
    0x12: assigned_client_identifier
    0x13: server_keep_alive
    0x15: authentication_method
    0x16: authentication_data
    0x17: request_problem_information
    0x18: will_delay_interval
    0x19: request_response_information
    0x1A: response_information
    0x1C: server_reference
    0x1F: reason_string
    0x21: receive_maximum
    0x22: topic_alias_maximum
    0x23: topic_alias
    0x24: maximum_qos
    0x25: retain_available
    0x26: user_property
    0x27: maximum_packet_size
    0x28: wildcard_subscription_available
    0x29: subscription_identifier_available
    0x2A: shared_subscription_available