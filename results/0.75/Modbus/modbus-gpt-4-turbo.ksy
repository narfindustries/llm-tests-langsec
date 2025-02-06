meta:
  id: modbus
  title: Modbus Protocol
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
        1: coils_response
        2: discrete_inputs_response
        3: holding_registers_response
        4: input_registers_response
        5: single_coil_response
        6: single_register_response
        15: multiple_coils_response
        16: multiple_registers_response
types:
  coils_response:
    seq:
      - id: byte_count
        type: u1
      - id: coil_status
        type: b1
        repeat: expr
        repeat-expr: (byte_count * 8)
  discrete_inputs_response:
    seq:
      - id: byte_count
        type: u1
      - id: input_status
        type: b1
        repeat: expr
        repeat-expr: (byte_count * 8)
  holding_registers_response:
    seq:
      - id: byte_count
        type: u1
      - id: registers
        type: u2
        repeat: expr
        repeat-expr: byte_count / 2
  input_registers_response:
    seq:
      - id: byte_count
        type: u1
      - id: input_registers
        type: u2
        repeat: expr
        repeat-expr: byte_count / 2
  single_coil_response:
    seq:
      - id: output_address
        type: u2
      - id: output_value
        type: u2
  single_register_response:
    seq:
      - id: register_address
        type: u2
      - id: register_value
        type: u2
  multiple_coils_response:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_outputs
        type: u2
      - id: byte_count
        type: u1
      - id: output_status
        type: b1
        repeat: expr
        repeat-expr: (byte_count * 8)
  multiple_registers_response:
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
        repeat-expr: byte_count / 2