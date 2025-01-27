meta:
  id: modbus
  file-extension: mdb
  endian: be

seq:
  - id: header
    type: header
  - id: function_code
    type: u1
  - id: data
    type: 
      switch-on: function_code
      cases:
        0x01: read_coils_response
        0x02: read_discrete_inputs_response
        0x03: read_holding_registers_response
        0x04: read_input_registers_response
        0x05: write_single_coil_response
        0x06: write_single_register_response
        0x0f: write_multiple_coils_response
        0x10: write_multiple_registers_response
        0x81: exception_response
        0x82: exception_response
        0x83: exception_response
        0x84: exception_response
        0x85: exception_response
        0x86: exception_response
        0x8f: exception_response
        0x90: exception_response

types:
  header:
    seq:
      - id: transaction_id
        type: u2
      - id: protocol_id
        type: u2
      - id: length
        type: u2
      - id: unit_id
        type: u1

  read_coils_response:
    seq:
      - id: byte_count
        type: u1
      - id: coil_status
        type: u1
        repeat: expr
        repeat-expr: byte_count

  read_discrete_inputs_response:
    seq:
      - id: byte_count
        type: u1
      - id: input_status
        type: u1
        repeat: expr
        repeat-expr: byte_count

  read_holding_registers_response:
    seq:
      - id: byte_count
        type: u1
      - id: register_value
        type: u2
        repeat: expr
        repeat-expr: byte_count / 2

  read_input_registers_response:
    seq:
      - id: byte_count
        type: u1
      - id: register_value
        type: u2
        repeat: expr
        repeat-expr: byte_count / 2

  write_single_coil_response:
    seq:
      - id: output_address
        type: u2
      - id: output_value
        type: u2

  write_single_register_response:
    seq:
      - id: register_address
        type: u2
      - id: register_value
        type: u2

  write_multiple_coils_response:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_outputs
        type: u2

  write_multiple_registers_response:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
        type: u2

  exception_response:
    seq:
      - id: exception_code
        type: u1

enums:
  function_codes:
    0x01: read_coils
    0x02: read_discrete_inputs
    0x03: read_holding_registers
    0x04: read_input_registers
    0x05: write_single_coil
    0x06: write_single_register
    0x0f: write_multiple_coils
    0x10: write_multiple_registers
    0x81: exception_read_coils
    0x82: exception_read_discrete_inputs
    0x83: exception_read_holding_registers
    0x84: exception_read_input_registers
    0x85: exception_write_single_coil
    0x86: exception_write_single_register
    0x8f: exception_write_multiple_coils
    0x90: exception_write_multiple_registers

  exception_codes:
    0x01: illegal_function
    0x02: illegal_data_address
    0x03: illegal_data_value
    0x04: slave_device_failure