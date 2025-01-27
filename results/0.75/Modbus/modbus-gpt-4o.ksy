meta:
  id: modbus
  title: Modbus Protocol
  endian: big

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
    type: switch
    cases:
      '1': read_coils
      '2': read_discrete_inputs
      '3': read_holding_registers
      '4': read_input_registers
      '5': write_single_coil
      '6': write_single_register
      '15': write_multiple_coils
      '16': write_multiple_registers

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
        repeat-expr: quantity_of_registers
