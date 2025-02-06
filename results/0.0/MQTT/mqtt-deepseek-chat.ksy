meta:
  id: mqtt
  title: MQTT Version 5.0
  license: CC0-1.0
  endian: be
seq:
  - id: fixed_header
    type: fixed_header
  - id: variable_header
    type: variable_header
    if: fixed_header.remaining_length > 0
  - id: payload
    size: 'fixed_header.remaining_length - (variable_header._io.size if variable_header._io else 0)'
    if: fixed_header.remaining_length > 0
types:
  fixed_header:
    seq:
      - id: packet_type
        type: u1
        enum: packet_types
      - id: flags
        type: u1
      - id: remaining_length
        type: vlq_base128_le
  variable_header:
    seq:
      - id: packet_identifier
        type: u2
        if: is_packet_with_identifier(fixed_header.packet_type)
      - id: properties_length
        type: vlq_base128_le
      - id: properties
        type: properties
        size: properties_length
  properties:
    seq:
      - id: properties_list
        type: property
        repeat: eos
  property:
    seq:
      - id: identifier
        type: u1
        enum: property_identifiers
      - id: value
        type:
          switch-on: identifier
          cases:
            property_identifiers::payload_format_indicator: u1
            property_identifiers::message_expiry_interval: u4
            property_identifiers::content_type: str_utf8
            property_identifiers::response_topic: str_utf8
            property_identifiers::correlation_data: bytes
            property_identifiers::subscription_identifier: vlq_base128_le
            property_identifiers::session_expiry_interval: u4
            property_identifiers::assigned_client_identifier: str_utf8
            property_identifiers::server_keep_alive: u2
            property_identifiers::authentication_method: str_utf8
            property_identifiers::authentication_data: bytes
            property_identifiers::request_problem_information: u1
            property_identifiers::will_delay_interval: u4
            property_identifiers::request_response_information: u1
            property_identifiers::receive_maximum: u2
            property_identifiers::topic_alias_maximum: u2
            property_identifiers::topic_alias: u2
            property_identifiers::maximum_qos: u1
            property_identifiers::retain_available: u1
            property_identifiers::user_property: user_property
            property_identifiers::maximum_packet_size: u4
            property_identifiers::wildcard_subscription_available: u1
            property_identifiers::subscription_identifier_available: u1
            property_identifiers::shared_subscription_available: u1
  user_property:
    seq:
      - id: key
        type: str_utf8
      - id: value
        type: str_utf8
  payload:
    seq:
      - id: client_identifier
        type: str_utf8
        if: fixed_header.packet_type == packet_types::connect
      - id: will_topic
        type: str_utf8
        if: fixed_header.packet_type == packet_types::connect
      - id: will_message
        type: bytes
        if: fixed_header.packet_type == packet_types::connect
      - id: username
        type: str_utf8
        if: fixed_header.packet_type == packet_types::connect
      - id: password
        type: bytes
        if: fixed_header.packet_type == packet_types::connect
      - id: topic_name
        type: str_utf8
        if: fixed_header.packet_type == packet_types::publish
      - id: message
        type: bytes
        if: fixed_header.packet_type == packet_types::publish
      - id: subscription_list
        type: subscription_list
        if: fixed_header.packet_type == packet_types::subscribe
      - id: return_codes
        type: return_codes
        if: fixed_header.packet_type == packet_types::connack || fixed_header.packet_type == packet_types::suback || fixed_header.packet_type == packet_types::unsuback
  subscription_list:
    seq:
      - id: topic_filters
        type: topic_filter
        repeat: eos
  topic_filter:
    seq:
      - id: topic_filter
        type: str_utf8
      - id: qos
        type: u1
  return_codes:
    seq:
      - id: codes
        type: u1
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
    23: request_problem_information
    24: will_delay_interval
    25: request_response_information
    26: receive_maximum
    27: topic_alias_maximum
    28: topic_alias
    29: maximum_qos
    30: retain_available
    31: user_property
    32: maximum_packet_size
    33: wildcard_subscription_available
    34: subscription_identifier_available
    35: shared_subscription_available