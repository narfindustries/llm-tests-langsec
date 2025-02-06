meta:
  id: mqtt
  file-extension: mqtt
  title: MQTT Protocol
  ks-version: 0.9

seq:
  - id: packet
    type: packet
    repeat: eos

types:
  packet:
    seq:
      - id: fixed_header
        type: fixed_header
      - id: variable_header
        type: switch-on
        switch-on: fixed_header.packet_type >> 4
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
          12: pingreq_variable_header
          13: pingresp_variable_header
          14: disconnect_variable_header
          15: auth_variable_header
      - id: payload
        size-eos: true

  fixed_header:
    seq:
      - id: packet_type
        type: u1
      - id: flags
        type: u1
        doc: Flags specific to each packet type
      - id: remaining_length
        type: vlq_base128_be

  connect_variable_header:
    seq:
      - id: protocol_name
        type: str
        size: 4
        encoding: UTF-8
      - id: protocol_level
        type: u1
      - id: connect_flags
        type: connect_flags
      - id: keep_alive
        type: u2be
      - id: properties
        size-eos: true

  connack_variable_header:
    seq:
      - id: session_present
        type: b1
      - id: reserved
        type: b7
      - id: reason_code
        type: u1
      - id: properties
        size-eos: true

  publish_variable_header:
    seq:
      - id: topic_name
        type: strz
        encoding: UTF-8
      - id: packet_identifier
        type: u2be
      - id: properties
        size-eos: true

  puback_variable_header:
    seq:
      - id: packet_identifier
        type: u2be
      - id: reason_code
        type: u1
      - id: properties
        size-eos: true

  pubrec_variable_header:
    seq:
      - id: packet_identifier
        type: u2be
      - id: reason_code
        type: u1
      - id: properties
        size-eos: true

  pubrel_variable_header:
    seq:
      - id: packet_identifier
        type: u2be
      - id: reason_code
        type: u1
      - id: properties
        size-eos: true

  pubcomp_variable_header:
    seq:
      - id: packet_identifier
        type: u2be
      - id: reason_code
        type: u1
      - id: properties
        size-eos: true

  subscribe_variable_header:
    seq:
      - id: packet_identifier
        type: u2be
      - id: properties
        size-eos: true

  suback_variable_header:
    seq:
      - id: packet_identifier
        type: u2be
      - id: properties
        size-eos: true

  unsubscribe_variable_header:
    seq:
      - id: packet_identifier
        type: u2be
      - id: properties
        size-eos: true

  unsuback_variable_header:
    seq:
      - id: packet_identifier
        type: u2be
      - id: properties
        size-eos: true

  pingreq_variable_header:
    seq:
      - id: properties
        size-eos: true

  pingresp_variable_header:
    seq:
      - id: properties
        size-eos: true

  disconnect_variable_header:
    seq:
      - id: reason_code
        type: u1
      - id: properties
        size-eos: true

  auth_variable_header:
    seq:
      - id: reason_code
        type: u1
      - id: properties
        size-eos: true

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
      - id: username_flag
        type: b1