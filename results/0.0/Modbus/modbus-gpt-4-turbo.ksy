meta:
  id: modbus
  title: Modbus Protocol
  application: Modbus
  endian: le
  license: CC0-1.0

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
    type:
      switch-on: function_code
      cases:
        1: coils_request
        2: coils_request
        3: register_request
        4: register_request
        5: single_coil
        6: single_register
        15: multiple_coils
        16: multiple_registers

types:
  coils_request:
    seq:
      - id: start_addr
        type: u2
      - id: quantity
        type: u2

  register_request:
    seq:
      - id: start_addr
        type: u2
      - id: quantity
        type: u2

  single_coil:
    seq:
      - id: output_addr
        type: u2
      - id: output_value
        type: u2

  single_register:
    seq:
      - id: register_addr
        type: u2
      - id: register_value
        type: u2

  multiple_coils:
    seq:
      - id: start_addr
        type: u2
      - id: quantity_outputs
        type: u2
      - id: byte_count
        type: u1
      - id: output_values
        type: b1
        repeat: expr
        repeat-expr: quantity_outputs

  multiple_registers:
    seq:
      - id: start_addr
        type: u2
      - id: quantity_regs
        type: u2
      - id: byte_count
        type: u1
      - id: register_values
        type: u2
        repeat: expr
        repeat-expr: quantity_regs