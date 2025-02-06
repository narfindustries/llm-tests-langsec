meta:
  id: mqtt_v5
  title: MQTT Version 5.0 Protocol
  file-extension: mqtt
  endian: be
  encoding: utf-8

types:
  varint:
    seq:
      - id: groups
        type: varint_group
        repeat: until
        repeat-until: not .has_next
    instances:
      value:
        value: >-
          groups[0].value
          + (groups.size >= 2 ? groups[1].value << 7 : 0)
          + (groups.size >= 3 ? groups[2].value << 14 : 0)
          + (groups.size >= 4 ? groups[3].value << 21 : 0)

  varint_group:
    seq:
      - id: has_next
        type: b1
      - id: value
        type: b7

  str_with_length:
    seq:
      - id: length
        type: u2
      - id: value
        type: str
        size: length
        encoding: utf-8

  bytes_with_length:
    seq:
      - id: length
        type: u2
      - id: value
        type: bytes
        size: length

  user_property:
    seq:
      - id: key
        type: str_with_length
      - id: value
        type: str_with_length

enums:
  packet_type:
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

  connect_flags:
    0: reserved
    1: username_present
    2: password_present
    4: will_retain
    8: will_qos_0
    16: will_qos_1
    24: will_qos_2
    32: will_flag
    64: clean_start

  property_type:
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
    38: maximum_qos
    39: retain_available
    40: user_properties
    41: maximum_packet_size
    42: wildcard_subscription_available
    43: subscription_identifiers_available
    44: shared_subscription_available

seq:
  - id: fixed_header
    type: fixed_header
  - id: variable_header
    type:
      switch-on: fixed_header.packet_type
      cases:
        'packet_type::connect': connect_packet
        'packet_type::connack': connack_packet
        'packet_type::publish': publish_packet
        'packet_type::puback': puback_packet
        'packet_type::pubrec': pubrec_packet
        'packet_type::pubrel': pubrel_packet
        'packet_type::pubcomp': pubcomp_packet
        'packet_type::subscribe': subscribe_packet
        'packet_type::suback': suback_packet
        'packet_type::unsubscribe': unsubscribe_packet
        'packet_type::unsuback': unsuback_packet
        'packet_type::disconnect': disconnect_packet
        'packet_type::auth': auth_packet

types:
  fixed_header:
    seq:
      - id: packet_type
        type: b4
        enum: packet_type
      - id: flags
        type: b4
      - id: remaining_length
        type: varint

  property:
    seq:
      - id: identifier
        type: u1
        enum: property_type
      - id: value
        type:
          switch-on: identifier
          cases:
            'property_type::payload_format_indicator': u1
            'property_type::message_expiry_interval': u4
            'property_type::content_type': str_with_length
            'property_type::response_topic': str_with_length
            'property_type::correlation_data': bytes_with_length
            'property_type::subscription_identifier': varint
            'property_type::session_expiry_interval': u4
            'property_type::assigned_client_identifier': str_with_length
            'property_type::server_keep_alive': u2
            'property_type::authentication_method': str_with_length
            'property_type::authentication_data': bytes_with_length
            'property_type::request_problem_information': u1
            'property_type::will_delay_interval': u4
            'property_type::request_response_information': u1
            'property_type::response_information': str_with_length
            'property_type::server_reference': str_with_length
            'property_type::reason_string': str_with_length
            'property_type::receive_maximum': u2
            'property_type::topic_alias_maximum': u2
            'property_type::topic_alias': u2
            'property_type::maximum_qos': u1
            'property_type::retain_available': u1
            'property_type::user_properties': user_property
            'property_type::maximum_packet_size': u4
            'property_type::wildcard_subscription_available': u1
            'property_type::subscription_identifiers_available': u1
            'property_type::shared_subscription_available': u1

  properties_container:
    seq:
      - id: length
        type: varint
      - id: properties
        type: property
        repeat: expr
        repeat-expr: length.value

  connect_packet:
    seq:
      - id: protocol_name
        type: str_with_length
      - id: protocol_version
        contents: [5]
      - id: connect_flags
        type: u1
        enum: connect_flags
      - id: keep_alive
        type: u2
      - id: properties
        type: properties_container
      - id: payload
        type: connect_payload

  connect_payload:
    seq:
      - id: client_identifier
        type: str_with_length
      - id: will_properties
        type: properties_container
        if: _parent.connect_flags.will_flag == 1
      - id: will_topic
        type: str_with_length
        if: _parent.connect_flags.will_flag == 1
      - id: will_payload
        type: bytes_with_length
        if: _parent.connect_flags.will_flag == 1
      - id: username
        type: str_with_length
        if: _parent.connect_flags.username_present == 1
      - id: password
        type: str_with_length
        if: _parent.connect_flags.password_present == 1

  connack_packet:
    seq:
      - id: flags
        type: u1
      - id: reason_code
        type: u1
      - id: properties
        type: properties_container

  publish_packet:
    seq:
      - id: topic_name
        type: str_with_length
      - id: packet_identifier
        type: u2
        if: _parent.fixed_header.flags & 0b0110 > 0
      - id: properties
        type: properties_container
      - id: payload
        type: bytes

  puback_packet:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties_container

  pubrec_packet:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties_container

  pubrel_packet:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties_container

  pubcomp_packet:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties_container

  subscribe_packet:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties_container
      - id: subscriptions
        type: subscription
        repeat: eos

  subscription:
    seq:
      - id: topic_filter
        type: str_with_length
      - id: subscription_options
        type: u1

  suback_packet:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties_container
      - id: reason_codes
        type: u1
        repeat: eos

  unsubscribe_packet:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties_container
      - id: topic_filters
        type: str_with_length
        repeat: eos

  unsuback_packet:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties_container
      - id: reason_codes
        type: u1
        repeat: eos

  disconnect_packet:
    seq:
      - id: reason_code
        type: u1
      - id: properties
        type: properties_container

  auth_packet:
    seq:
      - id: reason_code
        type: u1
      - id: properties
        type: properties_container