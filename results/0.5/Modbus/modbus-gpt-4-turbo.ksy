meta:
  id: modbus
  title: Modbus Protocol
  application: Modbus
  endian: le

seq:
  - id: transaction_id
    type: u2
  - id: protocol_id
    type: u2
  - id: length
    type: u2
  - id: unit_id
    type: u1
  - id: pdu
    type: pdu
    size: length - 1

types:
  pdu:
    seq:
      - id: function_code
        type: u1
      - id: data
        size-eos: true
        type:
          switch-on: function_code
          cases:
            1: coils_request
            2: discrete_inputs_request
            3: holding_registers_request
            4: input_registers_request
            5: write_single_coil_request
            6: write_single_register_request
            15: write_multiple_coils_request
            16: write_multiple_registers_request
            _: exception_response

  coils_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_coils
        type: u2

  discrete_inputs_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_inputs
        type: u2

  holding_registers_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
        type: u2

  input_registers_request:
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
        type: b1
        repeat: expr
        repeat-expr: byte_count

  write_multiple_registers_request:
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

  exception_response:
    seq:
      - id: exception_code
        type: u1