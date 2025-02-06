meta:
  id: modbus
  title: Modbus Protocol
  file-extension: modbus
  endian: be

seq:
  - id: mbap_header
    type: mbap_header
  - id: pdu
    type: protocol_data_unit

types:
  mbap_header:
    seq:
      - id: transaction_identifier
        type: u2
      - id: protocol_identifier
        type: u2
        valid: 0
      - id: length
        type: u2
      - id: unit_identifier
        type: u1

  protocol_data_unit:
    seq:
      - id: function_code
        type: u1
        enum: function_codes
      - id: payload
        type:
          switch-on: function_code
          cases:
            'function_codes::read_discrete_inputs': read_discrete_inputs
            'function_codes::read_coils': read_coils
            'function_codes::write_single_coil': write_single_coil
            'function_codes::write_multiple_coils': write_multiple_coils
            'function_codes::read_input_registers': read_input_registers
            'function_codes::read_holding_registers': read_holding_registers
            'function_codes::write_single_register': write_single_register
            'function_codes::write_multiple_registers': write_multiple_registers
            '_': generic_payload

  read_discrete_inputs:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_inputs
        type: u2

  read_coils:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_coils
        type: u2

  write_single_coil:
    seq:
      - id: output_address
        type: u2
      - id: output_value
        type: u2
        enum: coil_states

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

  read_input_registers:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
        type: u2

  read_holding_registers:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
        type: u2

  write_single_register:
    seq:
      - id: register_address
        type: u2
      - id: register_value
        type: u2

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

  generic_payload:
    seq:
      - id: raw_bytes
        type: u1
        repeat: eos

enums:
  function_codes:
    0x01: read_coils
    0x02: read_discrete_inputs
    0x03: read_holding_registers
    0x04: read_input_registers
    0x05: write_single_coil
    0x06: write_single_register
    0x0F: write_multiple_coils
    0x10: write_multiple_registers

  coil_states:
    0x0000: off
    0xFF00: on