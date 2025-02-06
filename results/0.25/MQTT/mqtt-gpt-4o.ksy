meta:
  id: mqtt
  title: MQTT Protocol
  file-extension: mqtt
  endian: be

seq:
  - id: packet
    type: packet

types:
  packet:
    seq:
      - id: fixed_header
        type: fixed_header
      - id: variable_header
        type:
          switch-on: fixed_header.packet_type
          cases:
            1: connect_variable_header
            2: connack_variable_header
            3: publish_variable_header
            4: puback_variable_header
            5: pubrec_variable_header
            6: pubrel_variable_header
            7: pubcomp_variable_header
            8: subscribe_variable_header
            9: suback_variable_header
            10: unsubscribe_variable_header
            11: unsuback_variable_header
            15: auth_variable_header
        if: fixed_header.packet_type != 13 and fixed_header.packet_type != 14
      - id: payload
        size: fixed_header.remaining_length - (variable_header._io.size if variable_header else 0)
        if: fixed_header.packet_type in [1, 3, 8, 10]

  fixed_header:
    seq:
      - id: packet_type
        type: b4
      - id: flags
        type: b4
      - id: remaining_length
        type: vlq_base128_be

  connect_variable_header:
    seq:
      - id: protocol_name
        type: str
        size: 4
        encoding: utf-8
      - id: protocol_level
        type: u1
      - id: connect_flags
        type: u1
      - id: keep_alive
        type: u2
      - id: properties
        type: properties

  connack_variable_header:
    seq:
      - id: session_present
        type: b1
      - id: connect_reason_code
        type: u1
      - id: properties
        type: properties

  publish_variable_header:
    seq:
      - id: topic_name
        type: strz
        encoding: utf-8
      - id: packet_identifier
        type: u2
        if: _parent.fixed_header.flags & 0x06 != 0
      - id: properties
        type: properties

  puback_variable_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  pubrec_variable_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  pubrel_variable_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  pubcomp_variable_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  subscribe_variable_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties

  suback_variable_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties

  unsubscribe_variable_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties

  unsuback_variable_header:
    seq:
      - id: packet_identifier
        type: u2
      - id: properties
        type: properties

  auth_variable_header:
    seq:
      - id: reason_code
        type: u1
      - id: properties
        type: properties

  properties:
    seq:
      - id: property_length
        type: vlq_base128_be
      - id: property_data
        type: bytes
        size: property_length