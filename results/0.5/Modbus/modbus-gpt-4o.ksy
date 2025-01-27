meta:
  id: modbus
  title: Modbus Protocol
  file-extension: modbus
  endian: be

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
        0x01: read_coils
        0x02: read_discrete_inputs
        0x03: read_holding_registers
        0x04: read_input_registers
        0x05: write_single_coil
        0x06: write_single_register
        0x0F: write_multiple_coils
        0x10: write_multiple_registers

types:
  read_coils:
    seq:
      - id: byte_count
        type: u1
      - id: coil_status
        type: b1
        repeat: expr
        repeat-expr: byte_count * 8

  read_discrete_inputs:
    seq:
      - id: byte_count
        type: u1
      - id: input_status
        type: b1
        repeat: expr
        repeat-expr: byte_count * 8

  read_holding_registers:
    seq:
      - id: byte_count
        type: u1
      - id: registers
        type: u2
        repeat: expr
        repeat-expr: byte_count / 2

  read_input_registers:
    seq:
      - id: byte_count
        type: u1
      - id: registers
        type: u2
        repeat: expr
        repeat-expr: byte_count / 2

  write_single_coil:
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
        repeat-expr: byte_count

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
        repeat-expr: byte_count / 2