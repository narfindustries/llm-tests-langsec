meta:
  id: modbus
  title: Modbus Protocol
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
        0x01: read_coils_request
        0x02: read_discrete_inputs_request
        0x03: read_holding_registers_request
        0x04: read_input_registers_request
        0x05: write_single_coil_request
        0x06: write_single_register_request
        0x0F: write_multiple_coils_request
        0x10: write_multiple_registers_request

types:
  read_coils_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_coils
        type: u2

  read_discrete_inputs_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_inputs
        type: u2

  read_holding_registers_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
        type: u2

  read_input_registers_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
        type: u2

  write_single_coil_request:
    seq:
      - id: output_address
        type: u2
      - id: output_value
        type: u2

  write_single_register_request:
    seq:
      - id: register_address
        type: u2
      - id: register_value
        type: u2

  write_multiple_coils_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_outputs
        type: u2
      - id: byte_count
        type: u1
      - id: output_values
        size: byte_count

  write_multiple_registers_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
        type: u2
      - id: byte_count
        type: u1
      - id: register_values
        size: byte_count