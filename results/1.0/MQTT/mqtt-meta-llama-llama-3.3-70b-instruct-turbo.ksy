meta:
  id: mqtt
  title: MQTT
  doc: MQTT (Message Queuing Telemetry Transport) protocol
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
        pos: byte_1
        type: hex
        enum: [1, 3, 8, 13]
        doc: Packet type (bit 4-7 of first byte)
      dup_flag:
        pos: byte_1
        type: bit
        doc: DUP flag (bit 3 of first byte)
      qos_level:
        pos: byte_1
        type: bit
        doc: QoS Level (bits 1-2 of first byte)
      retain_flag:
        pos: byte_1
        type: bit
        doc: RETAIN flag (bit 0 of first byte)
  remaining_length:
    seq:
      - id: length
        type: u1
meta:
  id: mqtt
  title: MQTT
  doc: MQTT (Message Queuing Telemetry Transport) protocol
seq:
  - id: header
    type: header
  - id: payload
    type: payload
    size: header.remaining_length.length
types:
  header:
    seq:
      - id: byte_1
        type: u1
      - id: remaining_length
        type: remaining_length
    instances:
      packet_type:
        pos: byte_1
        type: hex
        enum: [1, 3, 8, 13]
        doc: Packet type (bit 4-7 of first byte)
      dup_flag:
        pos: byte_1
        type: bit
        doc: DUP flag (bit 3 of first byte)
      qos_level:
        pos: byte_1
        type: bit
        doc: QoS Level (bits 1-2 of first byte)
      retain_flag:
        pos: byte_1
        type: bit
        doc: RETAIN flag (bit 0 of first byte)
  remaining_length:
    seq:
      - id: length
        type: u1
  payload:
    seq:
      - id: data
        type: u1
        repeat: expr
        repeat-expr: _root.header.remaining_length.length