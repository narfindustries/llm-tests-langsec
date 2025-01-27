meta:
  id: mqtt_packet
  title: MQTT Packet
  endian: be
  imports:
    - /path/to/length_prefixed_string

seq:
  - id: packet_type
    type: u1
    doc: |
      The first byte of an MQTT packet, containing the packet type
      and flags. The higher 4 bits represent the packet type, while
      the lower 4 bits are flags specific to each packet type.
  - id: remaining_length
    type: vlq_base128
    doc: |
      The remaining length of the packet after the fixed header,
      encoded using MQTT's variable-length encoding scheme.

types:
  vlq_base128:
    seq:
      - id: value
        type: u1
        repeat: until
        repeat-until: _.value < 0x80
    instances:
      total:
        value: 'value.map((b, i) => (b & 0x7f) << (7 * i)).reduce((a, b) => a + b)'

  length_prefixed_string:
    seq:
      - id: length
        type: u2
      - id: str
        size: length
        type: str
        encoding: utf-8
