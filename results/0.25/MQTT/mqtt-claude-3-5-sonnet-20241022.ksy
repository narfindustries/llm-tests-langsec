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

  vlq:
    seq:
      - id: groups
        type: u1
        repeat: until
        repeat-until: '(_ & 0x80) == 0'
    instances:
      value:
        value: >-
          (groups[0] & 0x7F) +
          (groups.size >= 2 ? ((groups[1] & 0x7F) << 7) : 0) +
          (groups.size >= 3 ? ((groups[2] & 0x7F) << 14) : 0) +
          (groups.size >= 4 ? ((groups[3] & 0x7F) << 21) : 0)

  variable_header:
    seq:
      - id: connect_header
        type: connect_header
        if: _parent.fixed_header.packet_type == packet_types::connect
      - id: connack_header
        type: connack_header
        if: _parent.fixed_header.packet_type == packet_types::connack
      - id: publish_header
        type: publish_header
        if: _parent.fixed_header.packet_type == packet_types::publish
      - id: properties
        type: properties
      - id: payload
        type: payload
        size-eos: true

  connect_header:
    seq:
      - id: protocol_name_length
        type: u2
      - id: protocol_name
        type: str
        size: protocol_name_length
        encoding: utf8
      - id: protocol_version
        type: u1
      - id: connect_flags
        type: u1
      - id: keep_alive
        type: u2
    instances:
      username_flag:
        value: (connect_flags & 0x80) >> 7
      password_flag:
        value: (connect_flags & 0x40) >> 6
      will_retain:
        value: (connect_flags & 0x20) >> 5
      will_qos:
        value: (connect_flags & 0x18) >> 3
      will_flag:
        value: (connect_flags & 0x04) >> 2
      clean_start:
        value: (connect_flags & 0x02) >> 1

  connack_header:
    seq:
      - id: acknowledge_flags
        type: u1
      - id: reason_code
        type: u1

  publish_header:
    seq:
      - id: topic_name_length
        type: u2
      - id: topic_name
        type: str
        size: topic_name_length
        encoding: utf8
      - id: packet_identifier
        type: u2
        if: qos > 0
    instances:
      dup:
        value: (_parent._parent.fixed_header.flags & 0x08) >> 3
      qos:
        value: (_parent._parent.fixed_header.flags & 0x06) >> 1
      retain:
        value: _parent._parent.fixed_header.flags & 0x01

  properties:
    seq:
      - id: length
        type: vlq
      - id: property_list
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
            'property_ids::content_type': utf8_string
            'property_ids::response_topic': utf8_string
            'property_ids::correlation_data': binary_data
            'property_ids::subscription_identifier': vlq
            'property_ids::session_expiry_interval': u4
            'property_ids::assigned_client_identifier': utf8_string
            'property_ids::server_keep_alive': u2
            'property_ids::authentication_method': utf8_string
            'property_ids::authentication_data': binary_data
            'property_ids::request_problem_information': u1
            'property_ids::will_delay_interval': u4
            'property_ids::request_response_information': u1
            'property_ids::response_information': utf8_string
            'property_ids::server_reference': utf8_string
            'property_ids::reason_string': utf8_string
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

  utf8_string:
    seq:
      - id: len_value
        type: u2
      - id: value
        type: str
        size: len_value
        encoding: utf8

  binary_data:
    seq:
      - id: len_value
        type: u2
      - id: value
        size: len_value

  user_property:
    seq:
      - id: key
        type: utf8_string
      - id: value
        type: utf8_string

  payload:
    seq:
      - id: data
        size-eos: true

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