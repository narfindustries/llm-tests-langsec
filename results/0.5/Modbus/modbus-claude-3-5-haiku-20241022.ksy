meta:
  id: modbus
  title: Modbus Protocol Specification
  file-extension: modbus
  endian: be

seq:
  - id: mbap_header
    type: mbap_header
  - id: pdu
    type: pdu

types:
  mbap_header:
    seq:
      - id: transaction_id
        type: u2
      - id: protocol_id
        type: u2
      - id: length
        type: u2
      - id: unit_id
        type: u1

  pdu:
    seq:
      - id: function_code
        type: u1
        enum: function_codes
      - id: data
        type:
          switch-on: function_code
          cases:
            'function_codes::read_coils': read_discrete_inputs_request
            'function_codes::read_discrete_inputs': read_discrete_inputs_request
            'function_codes::read_holding_registers': read_holding_registers_request
            'function_codes::read_input_registers': read_input_registers_request
            'function_codes::write_single_coil': write_single_coil_request
            'function_codes::write_single_register': write_single_register_request
            'function_codes::write_multiple_coils': write_multiple_coils_request
            'function_codes::write_multiple_registers': write_multiple_registers_request
            _: generic_response

  read_discrete_inputs_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity
        type: u2

  read_holding_registers_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity
        type: u2

  read_input_registers_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity
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
      - id: quantity
        type: u2
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
      - id: quantity
        type: u2
      - id: byte_count
        type: u1
      - id: register_values
        type: u2
        repeat: expr
        repeat-expr: quantity

  generic_response:
    seq:
      - id: raw_data
        type: u1
        repeat: eos

enums:
  function_codes:
    1: read_coils
    2: read_discrete_inputs
    3: read_holding_registers
    4: read_input_registers
    5: write_single_coil
    6: write_single_register
    15: write_multiple_coils
    16: write_multiple_registers