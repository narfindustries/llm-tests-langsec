meta:
  id: mqtt
  title: MQTT Protocol Version 5.0
  license: CC0-1.0
  endian: be
seq:
  - id: packet_type
    type: u1
    enum: packet_types
  - id: flags
    type: u1
    if: packet_type != 1 and packet_type != 14 and packet_type != 15
  - id: remaining_length
    type: vlq_base128_le
  - id: variable_header
    type: variable_header
    if: remaining_length > 0
  - id: payload
    type: payload
    if: remaining_length > 0 and (packet_type == 3 or packet_type == 8 or packet_type == 10)
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
    41: subscription_identifiers_available
    42: shared_subscription_available
types:
  vlq_base128_le:
    seq:
      - id: value
        type: u1
        repeat: until
        repeat-until: _io.is_eof or ((_ and 0x80) == 0)
  variable_header:
    seq:
      - id: packet_identifier
        type: u2
        if: _root.packet_type in [3, 4, 5, 6, 7, 8, 9, 10, 11]
      - id: properties
        type: properties
        if: _root.packet_type != 12 and _root.packet_type != 13
  properties:
    seq:
      - id: length
        type: vlq_base128_le
      - id: property
        type: property
        if: length.value > 0
  property:
    seq:
      - id: identifier
        type: u1
        enum: property_identifiers
      - id: value
        type:
          switch-on: identifier
          cases:
            1: u1
            2: u4
            3: utf8_str
            8: utf8_str
            9: bin_data
            11: vlq_base128_le
            17: u4
            18: utf8_str
            19: u2
            21: utf8_str
            22: bin_data
            23: u1
            24: u4
            25: u1
            26: utf8_str
            28: utf8_str
            31: utf8_str
            33: u2
            34: u2
            35: u2
            36: u1
            37: u1
            38: user_property
            39: u4
            40: u1
            41: u1
            42: u1
  user_property:
    seq:
      - id: key
        type: utf8_str
      - id: value
        type: utf8_str
  bin_data:
    seq:
      - id: length
        type: vlq_base128_le
      - id: data
        size: length.value
  utf8_str:
    seq:
      - id: length
        type: vlq_base128_le
      - id: value
        size: length.value
        encoding: UTF-8
  payload:
    seq:
      - id: data
        size: 'remaining_length - _io.pos'
        if: _root.packet_type == 3
      - id: subscribe_topics
        type: topic_list
        if: _root.packet_type == 8
      - id: unsubscribe_topics
        type: topic_list
        if: _root.packet_type == 10
  topic_list:
    seq:
      - id: topics
        type: topic
        repeat: eos
  topic:
    seq:
      - id: name
        type: utf8_str
      - id: qos
        type: u1
        if: _root.packet_type == 8