meta:
  id: modbus_deepseek_chat
  title: Modbus Protocol DeepSeek Chat
  license: MIT
  ks-version: 0.9
  endian: be

seq:
  - id: transaction_id
    type: u2
    doc: Transaction identifier
  - id: protocol_id
    type: u2
    doc: Protocol identifier (0 for Modbus/TCP)
  - id: length
    type: u2
    doc: Length of the remaining bytes
  - id: unit_id
    type: u1
    doc: Unit identifier
  - id: function_code
    type: u1
    doc: Function code
  - id: data
    type: u1
    repeat: expr
    repeat-expr: length - 2
    doc: Data field

types:
  u1:
    seq:
      - id: value
        type: u1
  u2:
    seq:
      - id: value
        type: u2