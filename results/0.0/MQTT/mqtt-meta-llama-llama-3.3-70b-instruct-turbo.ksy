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
        value: byte_0 & 0xf0
      dup:
        value: byte_1 & 0x08
      retain:
        value: byte_1 & 0x20
      packet_type:
        value: byte_0 & 0x0f

  variable_header:
    seq:
      - id: packet_identifier
        type: u2le
        if: packet_type in [3, 4, 7, 10, 11]
      - id: properties
        type: properties
        if: packet_type in [0, 2, 3, 4, 7, 8, 10, 11, 13]

  properties:
    seq:
      - id: property_length
        type: u2le
      - id: properties_list
        type: property
        repeat: expr
        repeat-expr: property_length

  property:
    seq:
      - id: property_id
        type: u1
      - id: property_value
        type:
          switch-on: property_id
          cases:
            0x01: # payload_format_indicator
              type: u1
            0x02: # message_expiry_interval
              type: u4le
            0x03: # content_type
              type: strz
            0x08: # response_topic
              type: strz
            0x09: # correlation_data
              type: bytes
            0x0b: # subscription_identifier
              type: u4le
            0x11: # session_expiry_interval
              type: u4le
            0x12: # assigned_client_identifier
              type: strz
            0x13: # server_keep_alive
              type: u2le
            0x15: # authentication_method
              type: strz
            0x16: # authentication_data
              type: bytes
            0x17: # request_problem_information
              type: u1
            0x19: # request_response_information
              type: u1
            0x1a: # response_information
              type: strz
            0x1c: # server_reference
              type: strz
            0x1f: # reason_string
              type: strz
            0x21: # receive_maximum
              type: u2le
            0x22: # topic_alias_maximum
              type: u2le
            0x23: # topic_alias
              type: u2le
            0x24: # maximum_qos
              type: u1
            0x25: # retain_available
              type: u1
            0x26: # user_property
              type:
                seq:
                  - id: key
                    type: strz
                  - id: value
                    type: strz
            0x27: # maximum_packet_size
              type: u4le
            0x28: # wildcard_subscription_available
              type: u1
            0x29: # subscription_identifier_available
              type: u1
            0x2a: # shared_subscription_available
              type: u1

  payload:
    seq:
      - id: payload_data
        type: bytes