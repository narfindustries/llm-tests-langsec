meta:
  id: modbus
  title: Modbus Protocol
  file-extension: modbus
  endian: le
  license: CC0-1.0
doc: |
  Modbus is a serial communications protocol originally published by Modicon (now Schneider Electric) in 1979 for use with its programmable logic controllers (PLCs). It has become a de facto standard communication protocol and is now a commonly available means of connecting industrial electronic devices.

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
    size: length - 2
    type:
      switch-on: function_code
      cases:
        1: coils
        2: discrete_inputs
        3: holding_registers
        4: input_registers
        5: single_coil
        6: write_single_register
        15: write_multiple_coils
        16: write_multiple_registers

types:
  coils:
    seq:
      - id: coil_status
        type: b1
        repeat: expr
        repeat-expr: _parent.length - 1

  discrete_inputs:
    seq:
      - id: input_status
        type: b1
        repeat: expr
        repeat-expr: _parent.length - 1

  holding_registers:
    seq:
      - id: register_values
        type: u2
        repeat: expr
        repeat-expr: (_parent.length - 1) / 2

  input_registers:
    seq:
      - id: register_values
        type: u2
        repeat: expr
        repeat-expr: (_parent.length - 1) / 2

  single_coil:
    seq:
      - id: output_address
        type: u2
      - id: output_value
        type: u2

  write_single_register:
    seq:
      - id: register_address
        type: u2
      - id: register_value
        type: u2

  write_multiple_coils:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_outputs
        type: u2
      - id: byte_count
        type: u1
      - id: output_values
        type: b1
        repeat: expr
        repeat-expr: quantity_of_outputs

  write_multiple_registers:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
        type: u2
      - id: byte_count
        type: u1
      - id: register_values
        type: u2
        repeat: expr
        repeat-expr: quantity_of_registers