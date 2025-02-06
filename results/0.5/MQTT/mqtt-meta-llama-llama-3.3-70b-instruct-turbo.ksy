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
      - id: byte_0
        type: u1
      - id: byte_1
        type: u1
    instances:
      qos:
        value: byte_0 & 0b00000111
      dup:
        value: byte_1 & 0b10000000
      retain:
        value: byte_1 & 0b01000000
      packet_type:
        value: byte_1 & 0b00001111

  variable_header:
    seq:
      - id: packet_identifier
        type: u2
        if: packet_type == 3 or packet_type == 4 or packet_type == 5 or packet_type == 6 or packet_type == 7 or packet_type == 8 or packet_type == 10 or packet_type == 11 or packet_type == 12 or packet_type == 13
      - id: protocol_name
        type: strz
        encoding: UTF-8
        if: packet_type == 1
      - id: protocol_level
        type: u1
        if: packet_type == 1
      - id: connect_flags
        type: u1
        if: packet_type == 1
      - id: keep_alive
        type: u2
        if: packet_type == 1
      - id: properties
        type: properties
        if: packet_type == 1 or packet_type == 2 or packet_type == 3 or packet_type == 4 or packet_type == 5 or packet_type == 8 or packet_type == 9 or packet_type == 10 or packet_type == 11 or packet_type == 13

  properties:
    seq:
      - id: authentication_method
        type: strz
        encoding: UTF-8
      - id: authentication_data
        type: bytes
      - id: authorization_data
        type: bytes
      - id: maximum_packet_size
        type: u4
      - id: receive_maximum
        type: u2
      - id: topic_alias_maximum
        type: u2
      - id: session_expiry_interval
        type: u4
      - id: assigned_client_identifier
        type: strz
        encoding: UTF-8
      - id: server_keep_alive
        type: u2
      - id: response_information
        type: strz
        encoding: UTF-8
      - id: server_reference
        type: strz
        encoding: UTF-8
      - id: reason_string
        type: strz
        encoding: UTF-8
      - id: user_property
        type: user_property

  user_property:
    seq:
      - id: key
        type: strz
        encoding: UTF-8
      - id: value
        type: strz
        encoding: UTF-8

  payload:
    seq:
      - id: will_message
        type: strz
        encoding: UTF-8
        if: connect_flags & 0b00010000
      - id: username
        type: strz
        encoding: UTF-8
        if: connect_flags & 0b10000000
      - id: password
        type: bytes
        if: connect_flags & 0b01000000
      - id: topic_name
        type: strz
        encoding: UTF-8
        if: packet_type == 3 or packet_type == 8
      - id: topic_alias
        type: u2
        if: packet_type == 3 or packet_type == 8
      - id: subscription_identifier
        type: u2
        if: packet_type == 10 or packet_type == 11 or packet_type == 12 or packet_type == 13
      - id: reason_code
        type: u1
        if: packet_type == 2 or packet_type == 4 or packet_type == 9 or packet_type == 11 or packet_type == 13
      - id: payload_format_indicator
        type: u1
        if: packet_type == 3 or packet_type == 8
      - id: payload_data
        type: bytes
        if: packet_type == 3 or packet_type == 8