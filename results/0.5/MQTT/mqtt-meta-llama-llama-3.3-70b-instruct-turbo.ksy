meta:
  id: mqtt
  title: MQTT
  file-format: binary
seq:
  - id: header
    type: header
types:
  header:
    seq:
      - id: byte_1
        type: u1
      - id: remaining_length
        type: u1
    instances:
      packet_type:
        pos: 0
        type: u4
        value: byte_1 >> 4
      dup_flag:
        pos: 0
        type: u1
        value: (byte_1 & 8) >> 3
      qos_level:
        pos: 0
        type: u2
        value: (byte_1 & 6) >> 1
      retain_flag:
        pos: 0
        type: u1
        value: byte_1 & 1
      remaining_length_value:
        pos: 1
        type: u4
        value: remaining_length