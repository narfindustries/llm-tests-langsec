meta:
  id: modbus
  endian: le
  title: Modbus Protocol
  file-extension: modbus
  license: CC0-1.0
  ks-version: 0.9

doc: |
  Modbus is a data communications protocol originally published by Modicon (now Schneider Electric) in 1979 for use with its programmable logic controllers (PLCs). Modbus has become a de facto standard communication protocol and is now a commonly available means of connecting industrial electronic devices.

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
        6: single_register
        15: multiple_coils
        16: multiple_registers

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
      - id: input_values
        type: u2
        repeat: expr
        repeat-expr: (_parent.length - 1) / 2

  single_coil:
    seq:
      - id: output_address
        type: u2
      - id: output_value
        type: b1

  single_register:
    seq:
      - id: register_address
        type: u2
      - id: register_value
        type: u2

  multiple_coils:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_outputs
        type: u2
      - id: output_status
        type: b1
        repeat: expr
        repeat-expr: quantity_of_outputs

  multiple_registers:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
        type: u2
      - id: register_values
        type: u2
        repeat: expr
        repeat-expr: quantity_of_registers