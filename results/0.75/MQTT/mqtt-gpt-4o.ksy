meta:
  id: mqtt
  title: MQTT
  file-extension: mqtt
  endian: be

seq:
  - id: packet
    type: packet
    repeat: eos

types:
  packet:
    seq:
      - id: fixed_header
        type: fixed_header
      - id: variable_header_payload
        type:
          switch-on: fixed_header.packet_type
          cases:
            1: connect_packet
            2: connack_packet
            3: publish_packet
            4: puback_packet
            5: pubrec_packet
            6: pubrel_packet
            7: pubcomp_packet
            8: subscribe_packet
            9: suback_packet
            10: unsubscribe_packet
            11: unsuback_packet
            12: pingreq_packet
            13: pingresp_packet
            14: disconnect_packet
            15: auth_packet

  fixed_header:
    seq:
      - id: packet_type
        type: u1
        doc: |
          The packet type and flags.
          The upper 4 bits represent the packet type.
      - id: remaining_length
        type: vlq_base128

  vlq_base128:
    seq:
      - id: bytes
        type: u1
        repeat: until
        repeat-until: _.bytes < 128
    instances:
      result:
        value: (bytes.map { it & 0x7f }).reduce(0) { acc, x -> (acc << 7) | x }

  connect_packet:
    seq:
      - id: protocol_name
        type: str
        size: 4
      - id: protocol_level
        type: u1
      - id: connect_flags
        type: connect_flags
      - id: keep_alive
        type: u2
      - id: properties
        type: properties
      - id: client_identifier
        type: strz
      - id: will_properties
        type: properties
        if: connect_flags.will_flag
      - id: will_topic
        type: strz
        if: connect_flags.will_flag
      - id: will_payload
        type: bytes
        if: connect_flags.will_flag
      - id: user_name
        type: strz
        if: connect_flags.user_name_flag
      - id: password
        type: bytes
        if: connect_flags.password_flag

    types:
      connect_flags:
        seq:
          - id: reserved
            type: b1
          - id: clean_start
            type: b1
          - id: will_flag
            type: b1
          - id: will_qos
            type: b2
          - id: will_retain
            type: b1
          - id: password_flag
            type: b1
          - id: user_name_flag
            type: b1

  connack_packet:
    seq:
      - id: connack_flags
        type: u1
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  publish_packet:
    seq:
      - id: topic_name
        type: strz
      - id: packet_identifier
        type: u2
        if: fixed_header.qos > 0
      - id: properties
        type: properties
      - id: payload
        type: bytes

  puback_packet:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  pubrec_packet:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  pubrel_packet:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  pubcomp_packet:
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
      - id: topic_filters
        type: topic_filter
        repeat: eos

    types:
      topic_filter:
        seq:
          - id: topic_filter
            type: strz
          - id: subscription_options
            type: subscription_options

      subscription_options:
        seq:
          - id: qos
            type: b2
          - id: no_local
            type: b1
          - id: retain_as_published
            type: b1
          - id: retain_handling
            type: b2

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
        type: strz
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

  pingreq_packet:
    seq: []

  pingresp_packet:
    seq: []

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

  properties:
    seq:
      - id: length
        type: vlq_base128
      - id: property
        type: property
        repeat: eos

  property:
    seq:
      - id: property_identifier
        type: u1
      - id: value
        type:
          switch-on: property_identifier
          cases:
            17: u4 # Session Expiry Interval
            33: u2 # Receive Maximum
            39: u4 # Maximum Packet Size
            34: u2 # Topic Alias Maximum
            25: u1 # Request Response Information
            23: u1 # Request Problem Information
            38: strz # Assigned Client Identifier
            37: strz # Reason String
            36: strz # User Property
            21: u1 # Wildcard Subscription Available
            22: u1 # Subscription Identifier Available
            19: u1 # Shared Subscription Available
            18: u2 # Server Keep Alive
            28: strz # Response Information
            30: strz # Server Reference
            24: strz # Authentication Method
            31: bytes # Authentication Data