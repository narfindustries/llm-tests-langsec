meta:
  id: mqtt
  title: MQTT
  endian: le

seq:
  - id: header
    type: mqtt_header

types:
  mqtt_header:
    seq:
      - id: byte_1
        type: u1
      - id: remaining_length
        type: u1

    instances:
      packet_type:
        value: byte_1 & 0xf0
      dup_flag:
        value: (byte_1 & 0x08) >> 3
      qos_level:
        value: (byte_1 & 0x03) >> 1
      retain_flag:
        value: byte_1 & 0x01

      payload_len:
        value: remaining_length