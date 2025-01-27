meta:
  id: mqtt_packet
  title: MQTT Packet
  file-extension: mqtt
  endian: be

seq:
  - id: fixed_header
    type: fixed_header

types:
  fixed_header:
    seq:
      - id: control_packet_type
        type: u1
        doc: |
          The MQTT Control Packet type. The higher nibble represents the packet type.
      - id: flags
        type: b4
        doc: |
          Flags specific to each MQTT Control Packet type.
      - id: remaining_length
        type: vlq_base128_be
        doc: |
          The length of the variable header and payload.

  vlq_base128_be:
    seq:
      - id: value
        type: u1
        repeat: until
        repeat-until: _.value < 0x80
    instances:
      length:
        value: 'value.size'
      total_value:
        value: |
          value.reduce((acc, byte, idx) => acc | ((byte & 0x7F) << (7 * idx)), 0)