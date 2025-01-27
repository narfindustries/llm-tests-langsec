meta:
  id: mqtt
  title: MQTT
  endian: le

seq:
  - id: header
    type: header

types:
  header:
    seq:
      - id: byte1
        type: u1
      - id: remaining_length
        type: u1
      - id: payload
        type: payload
        size: remaining_length

  payload:
    seq:
      - id: packet_type
        type: u1
      - id: dup
        type: u1
      - id: qos
        type: u1
      - id: retain
        type: u1
      - id: remaining_length
        type: u1
      - id: payload_data
        type: str
        size: remaining_length