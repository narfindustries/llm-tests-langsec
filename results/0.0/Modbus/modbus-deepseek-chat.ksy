meta:
  id: modbus
  endian: be
  license: CC0-1.0
  ks-version: 0.9
  imports:
    - /common/tcp_segment

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
        1: read_coils_response
        2: read_discrete_inputs_response
        3: read_holding_registers_response
        4: read_input_registers_response
        5: write_single_coil
        6: write_single_register
        15: write_multiple_coils
        16: write_multiple_registers

types:
  read_coils_response:
    seq:
      - id: byte_count
        type: u1
      - id: coil_status
        type: bytes
        size: byte_count

  read_discrete_inputs_response:
    seq:
      - id: byte_count
        type: u1
      - id: input_status
        type: bytes
        size: byte_count

  read_holding_registers_response:
    seq:
      - id: byte_count
        type: u1
      - id: register_values
        type: bytes
        size: byte_count

  read_input_registers_response:
    seq:
      - id: byte_count
        type: u1
      - id: register_values
        type: bytes
        size: byte_count

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
      - id: outputs_value
        type: bytes
        size: byte_count

  write_multiple_registers:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
        type: u2
      - id: byte_count
        type: u1
      - id: registers_value
        type: bytes
        size: byte_count