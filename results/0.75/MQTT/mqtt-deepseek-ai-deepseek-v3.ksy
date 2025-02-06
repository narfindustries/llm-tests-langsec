meta:
  id: mqtt
  title: MQTT v5.0
  license: CC0-1.0
  endian: be
seq:
  - id: fixed_header
    type: fixed_header
  - id: variable_header
    type: variable_header
    if: fixed_header.packet_type.value != 1 and fixed_header.packet_type.value != 14
  - id: payload
    type: payload
    if: fixed_header.packet_type.value == 3 or fixed_header.packet_type.value == 8 or fixed_header.packet_type.value == 10
types:
  fixed_header:
    seq:
      - id: packet_type
        type: u1
        enum: packet_types
      - id: flags
        type: u1
      - id: remaining_length
        type: u1
        repeat: until
        repeat-until: _ == 0 or _ < 128
  variable_header:
    seq:
      - id: properties
        type: properties
        if: fixed_header.packet_type.value != 12 and fixed_header.packet_type.value != 13
  properties:
    seq:
      - id: property_length
        type: u1
        repeat: until
        repeat-until: _ == 0 or _ < 128
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
            17: u2
            33: u4
            34: u4
            35: u4
            36: u4
            37: u4
            38: u2
            39: u2
            40: u2
            41: u2
            42: u2
            43: u1
            44: u1
            45: u1
            46: u1
            47: u1
            48: u1
            49: u1
            50: u1
            51: u1
            52: u1
            53: u1
            54: u1
            55: u1
            56: u1
            57: u1
            58: u1
            59: u1
            60: u1
            61: u1
            62: u1
            63: u1
            64: u1
            65: u1
            66: u1
            67: u1
            68: u1
            69: u1
            70: u1
            71: u1
            72: u1
            73: u1
            74: u1
            75: u1
            76: u1
            77: u1
            78: u1
            79: u1
            80: u1
            81: u1
            82: u1
            83: u1
            84: u1
            85: u1
            86: u1
            87: u1
            88: u1
            89: u1
            90: u1
            91: u1
            92: u1
            93: u1
            94: u1
            95: u1
            96: u1
            97: u1
            98: u1
            99: u1
            100: u1
            101: u1
            102: u1
            103: u1
            104: u1
            105: u1
            106: u1
            107: u1
            108: u1
            109: u1
            110: u1
            111: u1
            112: u1
            113: u1
            114: u1
            115: u1
            116: u1
            117: u1
            118: u1
            119: u1
            120: u1
            121: u1
            122: u1
            123: u1
            124: u1
            125: u1
            126: u1
            127: u1
  payload:
    seq:
      - id: data
        size-eos: true
enums:
  packet_types:
    0: reserved
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
    17: payload_format_indicator
    33: message_expiry_interval
    34: content_type
    35: response_topic
    36: correlation_data
    37: subscription_identifier
    38: session_expiry_interval
    39: assigned_client_identifier
    40: server_keep_alive
    41: authentication_method
    42: authentication_data
    43: request_problem_information
    44: will_delay_interval
    45: request_response_information
    46: response_information
    47: server_reference
    48: reason_string
    49: receive_maximum
    50: topic_alias_maximum
    51: topic_alias
    52: maximum_qos
    53: retain_available
    54: user_property
    55: maximum_packet_size
    56: wildcard_subscription_available
    57: subscription_identifier_available
    