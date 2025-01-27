meta:
  id: mqtt
  title: MQTT Protocol
  file-extension: mqtt
  endian: be
  license: CC0-1.0
seq:
  - id: header
    type: header
  - id: payload
    type: payload
    size-eos: true

types:
  header:
    seq:
      - id: message_type
        type: u1
        enum: message_type_enum
      - id: dup_flag
        type: b1
      - id: qos_level
        type: b2
      - id: retain
        type: b1
      - id: remaining_length
        type: vlq
  payload:
    seq:
      - id: topic
        type: string
        size: 2
      - id: message
        type: string
        size-eos: true

enums:
  message_type_enum:
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

types:
  vlq:
    seq:
      - id: bytes
        type: u1
        repeat: until
        repeat-until: _.last_byte == 0
    instances:
      value:
        value: 'bytes.map((byte, i) => (byte & 0x7F) << (7 * i)).reduce((a, b) => a + b)'
      last_byte:
        value: 'bytes[bytes.length - 1] & 0x80 == 0'
  string:
    seq:
      - id: length
        type: u2
      - id: data
        type: str
        size: length
        encoding: ASCII