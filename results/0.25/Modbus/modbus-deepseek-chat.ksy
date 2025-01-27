meta:
  id: modbus
  title: Modbus Protocol
  license: MIT
  endian: be
  imports:
    - /common/modbus_types

seq:
  - id: transaction_id
    type: u2
  - id: protocol_id
    type: u2
  - id: length
    type: u2
  - id: unit_id
    type: u1
  - id: function_code
    type: u1
  - id: data
    type: modbus_pdu
    size: length - 2

types:
  modbus_pdu:
    seq:
      - id: coils
        type: coil_array
        if: function_code == 1 or function_code == 2
      - id: registers
        type: register_array
        if: function_code == 3 or function_code == 4

  coil_array:
    seq:
      - id: byte_count
        type: u1
      - id: coil_status
        type: u1
        repeat: expr
        repeat-expr: byte_count

  register_array:
    seq:
      - id: byte_count
        type: u1
      - id: register_values
        type: u2
        repeat: expr
        repeat-expr: byte_count / 2