meta:
  id: mqtt
  title: MQTT Protocol
  file-extension: mqtt
  endian: be

seq:
  - id: fixed_header
    type: fixed_header

types:
  fixed_header:
    seq:
      - id: packet_type
        type: b4
      - id: flags
        type: b4
      - id: remaining_length
        type: remaining_length
      - id: variable_header_and_payload
        size: remaining_length.value
        type:
          switch-on: packet_type
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

  remaining_length:
    seq:
      - id: value
        type: u1
      - id: has_next
        type: bool
        size: 1
        if: value & 0x80 != 0
    instances:
      value:
        value: (value & 0x7F) + ((has_next ? 1 : 0) << 7)

  connect_packet:
    seq:
      - id: protocol_name
        type: utf8_string
      - id: protocol_level
        type: u1
      - id: connect_flags
        type: u1
      - id: keep_alive
        type: u2
      - id: properties
        type: properties
      - id: client_identifier
        type: utf8_string
      - id: will_properties
        type: properties
        if: connect_flags & 0x04 != 0
      - id: will_topic
        type: utf8_string
        if: connect_flags & 0x04 != 0
      - id: will_payload
        type: bytes
        size-eos: true
        if: connect_flags & 0x04 != 0
      - id: user_name
        type: utf8_string
        if: connect_flags & 0x80 != 0
      - id: password
        type: bytes
        size-eos: true
        if: connect_flags & 0x40 != 0

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
        type: utf8_string
      - id: packet_identifier
        type: u2
        if: (flags & 0x06) != 0
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
        type: topic_filters

  suback_packet:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties
      - id: reason_codes
        type: reason_codes

  unsubscribe_packet:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties
      - id: topic_filters
        type: topic_filters

  unsuback_packet:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties
      - id: reason_codes
        type: reason_codes

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
      - id: property_length
        type: remaining_length
      - id: property_data
        size: property_length.value
        type: property_list

  property_list:
    seq:
      - id: properties
        repeat: eos
        type: property

  property:
    seq:
      - id: property_identifier
        type: u1
      - id: property_value
        type:
          switch-on: property_identifier
          cases:
            1: u4
            2: u2
            3: u4
            4: u2
            5: u1
            6: u1
            7: utf8_string_pair
            8: utf8_string
            9: bytes

  utf8_string:
    seq:
      - id: length
        type: u2
      - id: value
        type: str
        encoding: utf-8
        size: length

  utf8_string_pair:
    seq:
      - id: key
        type: utf8_string
      - id: value
        type: utf8_string

  topic_filters:
    seq:
      - id: filters
        repeat: eos
        type: topic_filter

  topic_filter:
    seq:
      - id: topic
        type: utf8_string
      - id: options
        type: u1

  reason_codes:
    seq:
      - id: codes
        repeat: eos
        type: u1