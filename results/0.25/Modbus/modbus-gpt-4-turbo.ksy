meta:
  id: modbus_frame
  title: Modbus Protocol Frame
  endian: le
  license: CC0-1.0

seq:
  - id: transaction_id
    type: u2
    doc: Transaction Identifier for synchronization (Modbus TCP)
  - id: protocol_id
    type: u2
    doc: Zero for Modbus protocol (Modbus TCP)
  - id: length
    type: u2
    doc: Number of following bytes (Modbus TCP)
  - id: unit_id
    type: u1
    doc: Unit Identifier, similar to Device Address in RTU (Modbus TCP)
  - id: function_code
    type: u1
    doc: Function code determines the operation to perform
  - id: data
    type:
      switch-on: function_code
      cases:
        1: coils_req
        2: discrete_inputs_req
        3: holding_registers_req
        4: input_registers_req
        5: write_single_coil_req
        6: write_single_register_req
        15: write_multiple_coils_req
        16: write_multiple_registers_req
    doc: Data payload varies based on function code

types:
  coils_req:
    seq:
      - id: starting_addr
        type: u2
      - id: quantity_of_coils
        type: u2

  discrete_inputs_req:
    seq:
      - id: starting_addr
        type: u2
      - id: quantity_of_inputs
        type: u2

  holding_registers_req:
    seq:
      - id: starting_addr
        type: u2
      - id: quantity_of_registers
        type: u2

  input_registers_req:
    seq:
      - id: starting_addr
        type: u2
      - id: quantity_of_registers
        type: u2

  write_single_coil_req:
    seq:
      - id: output_addr
        type: u2
      - id: output_value
        type: u2
        enum: coil_value
        doc: 0xFF00 for ON and 0x0000 for OFF

  write_single_register_req:
    seq:
      - id: register_addr
        type: u2
      - id: register_value
        type: u2

  write_multiple_coils_req:
    seq:
      - id: starting_addr
        type: u2
      - id: quantity_of_outputs
        type: u2
      - id: byte_count
        type: u1
      - id: output_values
        type: b1
        repeat: expr
        repeat-expr: quantity_of_outputs

  write_multiple_registers_req:
    seq:
      - id: starting_addr
        type: u2
      - id: quantity_of_registers
        type: u2
      - id: byte_count
        type: u1
      - id: register_values
        type: u2
        repeat: expr
        repeat-expr: quantity_of_registers

enums:
  coil_value:
    0x0000: off
    0xFF00: on