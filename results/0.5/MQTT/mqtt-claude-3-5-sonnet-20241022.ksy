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
    size: fixed_header.remaining_length.value

types:
  fixed_header:
    seq:
      - id: packet_type_and_flags
        type: u1
      - id: remaining_length
        type: vlq
    instances:
      packet_type:
        value: (packet_type_and_flags & 0xF0) >> 4
        enum: packet_types
      flags:
        value: packet_type_and_flags & 0x0F

  variable_header:
    seq:
      - id: content
        type:
          switch-on: _parent.fixed_header.packet_type
          cases:
            'packet_types::connect': connect
            'packet_types::connack': connack
            'packet_types::publish': publish
            'packet_types::puback': puback
            'packet_types::pubrec': pubrec
            'packet_types::pubrel': pubrel
            'packet_types::pubcomp': pubcomp
            'packet_types::subscribe': subscribe
            'packet_types::suback': suback
            'packet_types::unsubscribe': unsubscribe
            'packet_types::unsuback': unsuback
            'packet_types::disconnect': disconnect
            'packet_types::auth': auth

  vlq:
    seq:
      - id: groups
        type: u1
        repeat: until
        repeat-until: '(_ & 0x80) == 0'
    instances:
      value:
        value: >-
          groups[0] & 0x7F +
          (groups[1] & 0x7F) * 128 +
          (groups[2] & 0x7F) * 16384 +
          (groups[3] & 0x7F) * 2097152

  mqtt_string:
    seq:
      - id: len_str
        type: u2
      - id: value
        type: str
        size: len_str
        encoding: UTF-8

  properties:
    seq:
      - id: length
        type: vlq
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
            'property_ids::subscription_identifier': vlq
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
      - id: key
        type: mqtt_string
      - id: value
        type: mqtt_string

  connect:
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
        type: mqtt_string
      - id: packet_identifier
        type: u2
        if: (_parent._parent.fixed_header.flags & 0x06) != 0
      - id: properties
        type: properties
      - id: payload
        size-eos: true

  puback:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  pubrec:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  pubrel:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  pubcomp:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  subscribe:
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
      - id: options
        type: u1

  suback:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties
      - id: reason_codes
        type: u1
        repeat: eos

  unsubscribe:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties
      - id: topic_filters
        type: mqtt_string
        repeat: eos

  unsuback:
    seq:
      - id: packet_identifier
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
    0x0b: subscription_identifier
    0x11: session_expiry_interval
    0x12: assigned_client_identifier
    0x13: server_keep_alive
    0x15: authentication_method
    0x16: authentication_data
    0x17: request_problem_information
    0x18: will_delay_interval
    0x19: request_response_information
    0x1a: response_information
    0x1c: server_reference
    0x1f: reason_string
    0x21: receive_maximum
    0x22: topic_alias_maximum
    0x23: topic_alias
    0x24: maximum_qos
    0x25: retain_available
    0x26: user_property
    0x27: maximum_packet_size
    0x28: wildcard_subscription_available
    0x29: subscription_identifier_available
    0x2a: shared_subscription_available