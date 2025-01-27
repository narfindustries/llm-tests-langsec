meta:
  id: mqtt
  title: MQTT Protocol
  license: CC0-1.0
  endian: be
  imports:
    - /common/fixed_contents

seq:
  - id: header
    type: header

  - id: payload
    type:
      switch-on: header.message_type
      cases:
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

types:
  fixed_header:
    seq:
      - id: message_type
        type: b4
      - id: dup_flag
        type: b1
      - id: qos_level
        type: b2
      - id: retain
        type: b1
      - id: remaining_length
        type: vlq

  header:
    seq:
      - id: fixed
        type: fixed_header

  connect:
    seq:
      - id: protocol_name
        type: strz
        encoding: UTF-8
      - id: version
        type: u1
      - id: flags
        type: u1
      - id: keep_alive
        type: u2
  
  connack:
    seq:
      - id: ack_flags
        type: u1
      - id: return_code
        type: u1

  publish:
    seq:
      - id: topic
        type: strz
        encoding: UTF-8
      - id: packet_id
        type: u2
    instances:
      payload:
        pos: _io.pos
        size: header.fixed.remaining_length - _io.pos + 2

  puback:
    seq:
      - id: packet_id
        type: u2

  pubrec:
    seq:
      - id: packet_id
        type: u2

  pubrel:
    seq:
      - id: packet_id
        type: u2

  pubcomp:
    seq:
      - id: packet_id
        type: u2
  
  subscribe:
    seq:
      - id: packet_id
        type: u2
      - id: topic_filters
        type: topic_filter
        repeat: eos

  topic_filter:
    seq:
      - id: topic
        type: strz
        encoding: UTF-8
      - id: requested_qos
        type: b2

  suback:
    seq:
      - id: packet_id
        type: u2
      - id: return_codes
        type: u1
        repeat: eos

  unsubscribe:
    seq:
      - id: packet_id
        type: u2
      - id: topics
        type: strz
        encoding: UTF-8
        repeat: eos

  unsuback:
    seq:
      - id: packet_id
        type: u2

  pingreq: {}
  pingresp: {}
  disconnect: {}

  vlq:
    seq:
      - id: bytes
        type: u1
        repeat: until
        repeat-until: _.value & 0x80 == 0
    instances:
      value:
        value: >
          bytes.reduce(0) { |sum, b| (sum << 7) | (b & 0x7f) }
