meta:
  id: modbus
  endian: be
  title: Modbus Protocol
  license: MIT

seq:
  - id: mbap_header
    type: mbap_header
    doc: Modbus Application Protocol Header
  - id: pdu
    type: protocol_data_unit

types:
  mbap_header:
    seq:
      - id: transaction_id
        type: u2
        doc: Identifies the transaction for request/response matching
      - id: protocol_id
        type: u2
        doc: Always 0 for Modbus
      - id: length
        type: u2
        doc: Number of following bytes
      - id: unit_id
        type: u1
        doc: Slave/Server address

  protocol_data_unit:
    seq:
      - id: function_code
        type: u1
        enum: function_codes
      - id: data
        type:
          switch-on: function_code
          cases:
            'function_codes::read_coils': read_coils_request
            'function_codes::read_discrete_inputs': read_discrete_inputs_request
            'function_codes::read_holding_registers': read_holding_registers_request
            'function_codes::read_input_registers': read_input_registers_request
            'function_codes::write_single_coil': write_single_coil_request
            'function_codes::write_single_register': write_single_register_request
            'function_codes::write_multiple_coils': write_multiple_coils_request
            'function_codes::write_multiple_registers': write_multiple_registers_request

  read_coils_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_coils
        type: u2
        valid:
          min: 1
          max: 2000

  read_discrete_inputs_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_inputs
        type: u2
        valid:
          min: 1
          max: 2000

  read_holding_registers_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
        type: u2
        valid:
          min: 1
          max: 125

  read_input_registers_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
        type: u2
        valid:
          min: 1
          max: 125

  write_single_coil_request:
    seq:
      - id: output_address
        type: u2
      - id: output_value
        type: u2
        enum: coil_values

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
      - id: quantity_of_coils
        type: u2
        valid:
          min: 1
          max: 1968
      - id: byte_count
        type: u1
      - id: coil_values
        type: u1
        repeat: expr
        repeat-expr: byte_count

  write_multiple_registers_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
        type: u2
        valid:
          min: 1
          max: 123
      - id: byte_count
        type: u1
      - id: register_values
        type: u2
        repeat: expr
        repeat-expr: quantity_of_registers

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

  coil_values:
    0x0000: off
    0xFF00: on