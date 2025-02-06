meta:
  id: modbus
  title: Modbus Protocol
  file-extension: modbus
  endian: be

seq:
  - id: mbap_header
    type: mbap_header
    if: is_tcp_encapsulation
  - id: pdu
    type: protocol_data_unit

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
            'function_codes::mask_write_register': mask_write_register_request
            'function_codes::read_write_multiple_registers': read_write_multiple_registers_request
            '_': exception_response

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
      - id: coil_address
        type: u2
      - id: coil_value
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
      - id: quantity_of_coils
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
      - id: quantity_of_registers
        type: u2
      - id: byte_count
        type: u1
      - id: register_values
        type: u2
        repeat: expr
        repeat-expr: quantity_of_registers

  mask_write_register_request:
    seq:
      - id: reference_address
        type: u2
      - id: and_mask
        type: u2
      - id: or_mask
        type: u2

  read_write_multiple_registers_request:
    seq:
      - id: read_starting_address
        type: u2
      - id: quantity_to_read
        type: u2
      - id: write_starting_address
        type: u2
      - id: quantity_to_write
        type: u2
      - id: write_byte_count
        type: u1
      - id: write_register_values
        type: u2
        repeat: expr
        repeat-expr: quantity_to_write

  exception_response:
    seq:
      - id: exception_code
        type: u1
        enum: exception_codes

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
    22: mask_write_register
    23: read_write_multiple_registers

  exception_codes:
    1: illegal_function
    2: illegal_data_address
    3: illegal_data_value
    4: slave_device_failure
    5: acknowledge
    6: slave_device_busy
    7: negative_acknowledge
    8: memory_parity_error

instances:
  is_tcp_encapsulation:
    value: true
    doc: Determines if the Modbus frame is encapsulated in TCP